import java.security.*;
import java.math.*;

public class Day4 {
    public static void main(String[] args) {
        new Day4().solve();
    }

    public void solve() {
        String input = "yzbqklnj";
        String[] tests = {"abcdef", "pqrstuv"};
        // for (String s : tests) {
        //     System.out.println(part1(s));
        // }
        System.out.println("Part 1: " + part1(input));
        System.out.println("Part 2: " + part2(input));
    }

    public String md5(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] messageDigest = md.digest(input.getBytes());
            BigInteger no = new BigInteger(1, messageDigest);
            String hashtext = no.toString(16);
            while (hashtext.length() < 32) {
                hashtext = "0" + hashtext;
            }
            return hashtext;
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    public int findBig(String input, String prefix) {
        int i = 0;
        while (!md5(input + i).startsWith(prefix)) {
            i++;
        }
        return i;
    }

    public int part1(String input) {
        return findBig(input, "00000");
    }

    public int part2(String input) {
        return findBig(input, "000000");
    }
}
