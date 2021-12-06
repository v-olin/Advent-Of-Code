using System;
using System.IO;
using System.Linq;

string[] instructions = File.ReadAllLines("init.txt");
string mask = "";
var memoryp1 = new List<Data>();
var memoryp2 = new List<Data>();

foreach (string s in instructions){
    string[] t = s.Split(' ');
    if (t[0].Contains("mask")) mask = t[2];
    else {
        t = s.Split(new char[] { '[', ']', ' ' });
        List<char> address = MaskAdress(ToBinaryString(t[1], 36), mask).ToCharArray().ToList();
        List<char[]> flucBits = PossibleCombinations(address.Where(c => c == 'X').Count());
        List<string> addressList = GetPossibleAdressList(address, flucBits, mask);
        WriteToMemory(ref memoryp1, t[1], t[4], mask);
        foreach (string a in addressList)
            WriteToMemory(ref memoryp2, a, t[4]);
    }
}

long ansp1 = memoryp1.Select(d => long.Parse(d.Value)).Sum();
long ansp2 = memoryp2.Select(d => long.Parse(d.Value)).Sum();
Console.WriteLine($"Part 1: {ansp1}\nPart 2: {ansp2}");

void WriteToMemory(ref List<Data> mem, string address, string value){
    List<Data> mad = mem.FindAll(d => d.Address == address);
    if (mad.Count() == 0) mem.Add(new Data() {
        Address = address,
        Value = value
    });
    else mem[mem.IndexOf(mad.First())].Value = value;
}

void WriteToMemory(ref List<Data> mem, string address, string val, string mask){
    char[] value = ToBinaryString(val, 36).ToCharArray();
    for (int i = 0; i < value.Count(); i++){
        value[i] = mask[i] == 'X' ? value[i] : mask[i];
    }
    string valToWrite = Convert.ToInt64(new string (value), 2).ToString();
    WriteToMemory(ref mem, address, valToWrite);
}

string MaskAdress(string address, string mask){
    char[] add = address.ToCharArray();
    for (int i = 0; i < mask.Length; i++)
        add[i] = mask[i] != '0' ? mask[i] : add[i];
    return new string(add);
}

List<string> GetPossibleAdressList(List<char> address, List<char[]> combinations, string mask){
    var newAddressList = new List<string>();
    var add = new List<char>(address);
    for (int i = 0; i < combinations.Count(); i++){
        for (int j = 0; j < combinations[0].Count(); j++){
            add[add.IndexOf('X')] = combinations[i][j];
        }
        newAddressList.Add(new string(add.ToArray()));
        add = new List<char>(address);
    }
    return newAddressList
        .Select(a => Convert.ToInt64(a , 2).ToString())
        .ToList();
}

string ToBinaryString(string s, int l){
    var sInBinary = Convert.ToString(int.Parse(s), 2);
    return sInBinary.Length < l ? (new string('0', l - sInBinary.Length) + sInBinary) : sInBinary;
}

List<char[]> PossibleCombinations(int n){
    return Enumerable.Range(0, (int)Math.Pow(2, n))
        .Select(n => Convert.ToString(n))
        .Select(s => ToBinaryString(s, n))
        .Select(s => s.ToCharArray())
        .ToList();
}

public class Data {
    public string Address { get; set; }
    public string Value { get; set; }
}