// libc++ fstream and std::filesystem on SLOW-32, in the current directory
// (the gate runs each program in a scratch directory of its own).
// Self-checking; prints what a host build of this file prints -- relative
// paths only, since absolute ones differ between the two.
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <system_error>
#include <vector>

namespace fs = std::filesystem;

static int fails;
static void check(const char *what, bool ok) {
    std::cout << what << (ok ? " PASS" : " FAIL") << '\n';
    if (!ok) fails++;
}

static std::string slurp(const fs::path &p) {
    std::ifstream in(p);
    std::string all, line;
    while (std::getline(in, line)) all += line + "|";
    return all;
}

int main() {
    const fs::path root = "files_scratch";
    std::error_code ec;
    fs::remove_all(root, ec);

    // ---- paths, without touching the disk
    fs::path p = fs::path("shelf") / "jars" / "honey.txt";
    check("path composition", p.generic_string() == "shelf/jars/honey.txt");
    check("filename / stem / extension",
          p.filename() == "honey.txt" && p.stem() == "honey" && p.extension() == ".txt");
    check("parent_path", p.parent_path().generic_string() == "shelf/jars");
    check("lexically_normal", fs::path("a/./b/../c").lexically_normal().generic_string() == "a/c");
    check("replace_extension", fs::path("notes.md").replace_extension(".txt") == "notes.txt");

    // ---- directories and fstream
    check("create_directories", fs::create_directories(root / "sub" / "deeper"));
    check("is_directory", fs::is_directory(root / "sub") && !fs::is_regular_file(root / "sub"));
    {
        std::ofstream out(root / "a.txt");
        out << "first\nsecond\nthird\n";
        check("ofstream writes", out.good());
    }
    check("ifstream reads", slurp(root / "a.txt") == "first|second|third|");
    check("file_size", fs::file_size(root / "a.txt") == 19);
    {
        std::ofstream app(root / "a.txt", std::ios::app);
        app << "fourth\n";
    }
    check("append mode", slurp(root / "a.txt") == "first|second|third|fourth|");
    {
        std::fstream both(root / "a.txt", std::ios::in | std::ios::out);
        both.seekp(0);
        both << "FIRST";
        both.seekg(0);
        std::string w;
        both >> w;
        check("fstream read and write", w == "FIRST");
    }
    {
        std::ofstream bin(root / "b.bin", std::ios::binary);
        for (int i = 0; i < 256; i++) bin.put(static_cast<char>(i));
    }
    {
        std::ifstream bin(root / "b.bin", std::ios::binary);
        std::vector<char> data((std::istreambuf_iterator<char>(bin)), std::istreambuf_iterator<char>());
        check("binary round trip", data.size() == 256 && static_cast<unsigned char>(data[200]) == 200);
    }
    check("missing file does not open", !std::ifstream(root / "nowhere.txt").is_open());

    // ---- copy, rename, list, remove
    check("copy_file", fs::copy_file(root / "a.txt", root / "sub" / "copy.txt") &&
                           slurp(root / "sub" / "copy.txt") == slurp(root / "a.txt"));
    fs::rename(root / "b.bin", root / "sub" / "moved.bin");
    check("rename", !fs::exists(root / "b.bin") && fs::exists(root / "sub" / "moved.bin"));
    std::vector<std::string> names;
    for (const auto &e : fs::directory_iterator(root / "sub")) names.push_back(e.path().filename().string());
    std::sort(names.begin(), names.end());
    std::string listed;
    for (auto &n : names) listed += n + " ";
    check("directory_iterator", listed == "copy.txt deeper moved.bin ");
    int deep = 0;
    for (const auto &e : fs::recursive_directory_iterator(root)) { (void)e; deep++; }
    check("recursive_directory_iterator", deep == 5);
    check("remove a file", fs::remove(root / "sub" / "copy.txt") && !fs::exists(root / "sub" / "copy.txt"));
    check("remove what is not there", !fs::remove(root / "sub" / "copy.txt"));

    // ---- where we are
    fs::path here = fs::current_path();
    check("current_path is absolute", here.is_absolute());
    check("absolute", fs::absolute(root) == here / root);
    check("canonical", fs::canonical(root / "sub" / ".." / "a.txt") == here / root / "a.txt");
    fs::current_path(root / "sub");
    check("current_path changes", fs::current_path() == here / root / "sub" && fs::exists("moved.bin"));
    fs::current_path(here);

    // ---- errors come back as error codes, or exceptions
    fs::file_size(root / "nowhere.txt", ec);
    check("error_code for a missing file", ec == std::errc::no_such_file_or_directory);
    bool threw = false;
    try { (void)fs::file_size(root / "nowhere.txt"); } catch (fs::filesystem_error &e) {
        threw = e.code() == std::errc::no_such_file_or_directory;
    }
    check("filesystem_error for a missing file", threw);

    check("remove_all", fs::remove_all(root) == 5 && !fs::exists(root));
    std::cout << fails << " failed\n";
    return fails;
}
