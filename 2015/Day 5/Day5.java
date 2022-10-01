import java.io.*;
import java.util.*;

public class Day5 {
    public static void main(String[] args) {
        new Day5().solve();
    }

    public void solve() {
        String inputFile = "input.txt";
        List<String> data = readData(inputFile);
        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
    }

    public int part1(List<String> data) {
        return data.stream().mapToInt(s -> isNice(s, true) ? 1 : 0).sum();
    }

    public int part2(List<String> data) {
        return data.stream().mapToInt(s -> isNice(s, false) ? 1 : 0).sum();
    }

    public boolean isNice(String s, boolean part1) {
        if (part1)
            return containsVowels(s) && containsDouble(s) && !containsBad(s);
        return containsPair(s) && containsRepeat(s);
    }

    public boolean containsVowels(String s) {
        String vowels = "aeiou";
        int count = 0;
        for (int i = 0; i < s.length(); i++) {
            if (vowels.indexOf(s.charAt(i)) != -1) {
                count++;
            }
        }
        return count >= 3;
    }

    public boolean containsDouble(String s) {
        for (int i = 0; i < s.length() - 1; i++) {
            if (s.substring(i, i + 1).equals(s.substring(i + 1, i + 2))) {
                return true;
            }
        }
        return false;
    }

    public boolean containsBad(String s) {
        String[] bad = { "ab", "cd", "pq", "xy" };
        for (int i = 0; i < bad.length; i++) {
            if (s.contains(bad[i])) {
                return true;
            }
        }
        return false;
    }

    public boolean containsPair(String s) {
        for (int i = 0; i < s.length() - 1; i++) {
            String pair = s.substring(i, i + 2);
            if (s.indexOf(pair, i + 2) != -1) {
                return true;
            }
        }
        return false;
    }

    public boolean containsRepeat(String s) {
        for (int i = 0; i < s.length() - 2; i++) {
            if (s.charAt(i) == s.charAt(i + 2)) {
                return true;
            }
        }
        return false;
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
