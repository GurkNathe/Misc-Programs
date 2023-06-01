import java.io.*;
import java.util.*;

public class RandomFile {
  public static void main(String[] args) {
    PrintWriter writer = null;
    try {
      /* Create a new file with UTF-8 encoding */
      writer = new PrintWriter("consp.txt", "UTF-8");

      String x = "";
      Random r = new Random(51354843l);
      for (int i = 0; i < 1000000; i++) {
        int n = r.nextInt(10000);
        while (n == 0) {
          n = r.nextInt(10000);
        }
        x += (char) r.nextInt(n);
      }
      writer.println(x);

      writer.close();
      System.out.println("New File Created!");
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (UnsupportedEncodingException e) {
      e.printStackTrace();
    }
  }
}
