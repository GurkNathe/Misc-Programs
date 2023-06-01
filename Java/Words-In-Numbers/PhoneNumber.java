import java.util.*;
import java.lang.*;

/*
* Used to see if a list of given words is contained in a given phone number.
*/
public class PhoneNumber{

   //Map that holds the alphabet to phone pad conversions.
   private Map<String, String> letterToNumber;
   //List of words filtered based on a given phone number.
   private static ArrayList<String> filterWords;
   //List of given words.
   private static String[] words;
      
   public PhoneNumber(String number, String[] list){
      letterToNumber = makeMap();
      words = list;
      filterWords = contained(number, convertWords(list), words);
      Collections.sort(filterWords, String.CASE_INSENSITIVE_ORDER);
   }
   
   //Creates a Map to handle which number contains which letters.
   private Map<String, String> makeMap(){
      Map<String, String> yes = new HashMap<String, String>();
      String alphabet = "abcdefghijklmnopqrstuvwxyz";
      int[] order = {2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,7,8,8,8,9,9,9,9};
      for(int i = 0; i < alphabet.length(); i++)
         yes.put(alphabet.substring(i,i+1), Integer.toString(order[i]));
      return yes;
   }
   
   //Converts the given list of words into a list of numbers according to
   //how they appear on a number pad for a phone.
   private String[] convertWords(String[] list){
      if(list.length == 0)
         return null;
      String[] nums = new String[list.length];
      for(int i = 0; i < list.length; i++){
         String num = "";
         for(int j = 0; j < list[i].length(); j++){
            num += letterToNumber.get(list[i].substring(j, j+1));
         }
         nums[i] = num;
      }
      return nums;
   }
   
   //Helper method to sort the given list of words to see if they are
   //in the phone number given
   private ArrayList<String> contained(String name, String[] list, String[] words){
      ArrayList<String> inWords = new ArrayList<String>();
      for(int i = 0; i < list.length; i++){
         if(name.contains(list[i]))
            inWords.add(words[i]);
      }
      return inWords;
   }
   
   /*
   * Returns the unfiltered list of given words.
   */
   public static String[] getWords(){
      return words;
   }
   
   /*
   * Returns the filtered list of given words, based on the phone number given.
   */
   public static ArrayList<String> getFilterWords(){
      return filterWords;
   }
   
   /*
   * Test method.
   */
   public static void main(String[]args){
      while(true){
         Scanner s = new Scanner(System.in);
         System.out.print("Enter a phone number (enter 'end' or 'stop' to exit): ");
         String num = s.nextLine();
         if(num.equalsIgnoreCase("end") || num.equalsIgnoreCase("stop"))
            break;
         System.out.print("Enter a comma seperated list of words: ");
         String[] list = s.nextLine().split(",");
         for(int i = 0; i < list.length; i++)
            list[i] = list[i].trim();
         PhoneNumber p = new PhoneNumber(num, list);
         System.out.println("Words contained in the phone number: " + p.getFilterWords());
      }
   }
}