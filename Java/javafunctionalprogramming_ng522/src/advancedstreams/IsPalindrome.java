package advancedstreams;

public class IsPalindrome {

  public static boolean isPalindrome(String string) {
    for (int i = 0; i < string.length() / 2; i++) {
      if (string.charAt(i) != string.charAt(string.length() - 1 - i)) {
        return false;
      }
    }
    return true;
  }
}
