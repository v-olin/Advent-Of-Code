import java.io.*;

public class Day1 {
    public static void main(String[] args) {
        String inputFile = "/input.txt";
        InputStream dataStream = Day1.class.getResourceAsStream(inputFile);
        String data = readFromInputStream(dataStream);
        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
    }

    private static int part1(String data) {
        int floor = 0;
        for (int i = 0; i < data.length(); i++) {
            char c = data.charAt(i);
            if (c == '(') {
                floor++;
            } else if (c == ')') {
                floor--;
            }
        }
        return floor;
    }

    private static int part2(String data) {
        int floor = 0, i = 0;
        while (i < data.length()) {
            char c = data.charAt(i);
            if (c == '(') {
                floor++;
            } else if (c == ')') {
                floor--;
            }
            if (floor < 0) {
                return i+1;
            }
            i++;
        }
        return -1;
    }

    private static String readFromInputStream(InputStream inputStream) {
        StringBuilder resultStringBuilder = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = br.readLine()) != null) {
                resultStringBuilder.append(line).append("\n");
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
        return resultStringBuilder.toString();
    }
}