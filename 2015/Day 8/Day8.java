import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.stream.*;

public class Day8 {
    public static void main(String[] args) {
        new Day8().solve();
    }

    public void solve() {
        String input = "input.txt";
        List<String> lines = readData(input);

        int[] result = calc(lines);

        System.out.println("Part 1: " + result[0]);
        System.out.println("Part 2: " + result[1]);
    }

    public int[] calc(List<String> lines) {
        int pA = 0, pB = 0;

        for (String s : lines) {
            pA += s.length() - memLength(s);
            pB += encodeLength(s) - s.length();
        }

        return new int[] { pA, pB };
    }

    public int encodeLength(String s) {
        int len = 2;

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '\\' || c == '"') {
                len++;
            }
            len++;
        }
        
        return len;
    }

    public int memLength(String s) {
        int length = 0;
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != '"' && s.charAt(i) == '\\') {
                if (s.charAt(i + 1) == 'x') {
                    length += 1;
                    i += 3;
                } else {
                    length += 1;
                    i += 1;
                }
            } else if (s.charAt(i) != '"') {
                length += 1;
            }
        }
        return length;
    }

    public List<String> readData(String file) {
        List<String> data = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = br.readLine()) != null) {
                data.add(line);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
        return data;
    }
}
