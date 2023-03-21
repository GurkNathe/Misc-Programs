import cv2

import smtplib
import ssl

from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

import time
from datetime import datetime

import os
from os.path import basename

from dotenv import load_dotenv

from providers import PROVIDERS


def detect_person(
    SECONDS_TILL_STOP: int = 10,
    frame_num: int = 20,
    face_accuracy: float = 1.1,
    body_accuracy: float = 1.1,
    face_neighbors: int = 5,
    body_neighbors: int = 5,
    show_camera: bool = True,
    camera_name: int = 0,
    frame_size: tuple = (640, 480)
):
    cap = cv2.VideoCapture(camera_name)

    # Create classifiers for detection faces and bodies
    face_cascade = cv2.CascadeClassifier(
        os.path.join(cv2.data.haarcascades, "haarcascade_frontalface_default.xml"))
    body_cascade = cv2.CascadeClassifier(
        os.path.join(cv2.data.haarcascades, "haarcascade_fullbody.xml"))

    # Initialize variables
    detection: bool = False
    detection_stopped_time: bool = None
    timer_started: bool = False

    frame_count: int = 0
    frame_time = datetime.now().strftime("%d-%m-%Y-%H-%M-%S")

    fourCC = cv2.VideoWriter_fourcc(*"mp4v")

    while True:
        _, frame = cap.read()

        # Save frame
        if frame_count == frame_num:
            cv2.imwrite(f"./{frame_time}.jpg", frame)

        # Convert to gray-scale and analyze for faces and bodies
        gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        faces = face_cascade.detectMultiScale(
            gray, face_accuracy, face_neighbors)
        bodies = body_cascade.detectMultiScale(
            gray, body_accuracy, body_neighbors)

        # Determine if and faces or bodies were detected in the current frame
        if len(faces) + len(bodies) > 0:
            if detection:
                timer_started = False
            else:

                # If a newly detected face/body appears in frame, create and start a new recording
                detection = True
                current_time = datetime.now().strftime("%d-%m-%Y-%H-%M-%S")
                out = cv2.VideoWriter(
                    f"{current_time}.mp4", fourCC, 30, frame_size)
                print("Started recording")
        elif detection:

            # If face/body was previously detected and the countdown to designated end buffer has started
            if timer_started:

                # If the buffer time has been reached
                if detection_stopped_time != None and \
                        time.time() - detection_stopped_time >= SECONDS_TILL_STOP:

                    # Reset variables
                    detection = False
                    timer_started = False
                    detection_stopped_time = None
                    out.release()
                    time.sleep(1)

                    # Send screenshot of recording to designated number
                    send_recording(
                        os.getenv('NUMBER'),
                        "Sending screenshot.",
                        f"./{frame_time}.jpg",
                        "image",
                        "jpg",
                        os.getenv('PROVIDER'),
                        os.getenv('PROVIDER_SCHEMA'),
                        (os.getenv('EMAIL'), os.getenv('PASSWORD')),
                        "Face/Body detected"
                    )
                    print("Stopped recording")
            else:
                # Start countdown timer
                timer_started = True
                detection_stopped_time = time.time()

        if detection:
            out.write(frame)

        if show_camera:
            # Formating detected faces/bodies to show them on the camera
            for (x, y, width, height) in faces:
                cv2.rectangle(frame, (x, y), (x + width,
                                              y + height), (255, 0, 0), 3)

            for (x, y, width, height) in bodies:
                cv2.rectangle(frame, (x, y), (x + width,
                                              y + height), (0, 0, 255), 3)

            cv2.imshow("Camera", frame)

            # Break key for when the camera is displayed
            if cv2.waitKey(1) == ord('q'):
                break

        frame_count += 1

    out.release()
    cap.release()
    cv2.destroyAllWindows()


def send_recording(
    number: str,
    message: str,
    file_path: str,
    mime_maintype: str,
    mime_subtype: str,
    provider: str,
    provider_schema: str,
    sender_credentials: tuple,
    subject: str = "Default Subject",
    smtp_server: str = "smtp.gmail.com",
    smtp_port: int = 465
):
    # Initialize variables
    sender_email, email_password = sender_credentials
    receiver_email = f"{number}@{PROVIDERS.get(provider).get(provider_schema)}"

    email_message = MIMEMultipart()
    email_message["Subject"] = subject
    email_message["From"] = sender_email
    email_message["To"] = receiver_email

    email_message.attach(MIMEText(message, "plain"))

    # Open file to send and format it to be sent
    with open(file_path, "rb") as attachment:
        part = MIMEBase(mime_maintype, mime_subtype)
        part.set_payload(attachment.read())

        encoders.encode_base64(part)
        part.add_header(
            "Content-Disposition",
            f"attachment; filename={basename(file_path)}",
        )

        email_message.attach(part)

    text = email_message.as_string()

    # Send the message
    with smtplib.SMTP_SSL(
        smtp_server, smtp_port, context=ssl.create_default_context()
    ) as email:
        email.login(sender_email, email_password)
        email.sendmail(sender_email, receiver_email, text)


if __name__ == "__main__":
    load_dotenv(dotenv_path="./config.env")
    detect_person()
