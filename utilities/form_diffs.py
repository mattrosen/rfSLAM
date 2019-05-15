"""
    form_diffs.py

    Author: Matt Rosen
    Modified: 4/2019

    Description: Form diffs file-by-file for ease.
"""

import glob
import os
import queue
import subprocess

def form_diffs_for_repo(repo_new, repo_orig):

    # set up logging directory
    if not os.path.exists(repo_new + "_diffs/"):
        os.makedirs(repo_new + "_diffs/")

    log_dir = f"{repo_new}_diffs"

    # make a queue: if a fn is a folder, add all files inside
    # to the file queue; add folder to the folder queue
    folder_queue = queue.SimpleQueue()

    # pull all the folders in both repos and enqueue
    new_folders = glob.glob(repo_new + "/*/")
    print(glob.glob(repo_orig + "/*/"))

    # add folders
    for i in new_folders:
        folder_queue.put(os.path.basename(os.path.dirname(i)))

    # proceed through folders in queue
    while not folder_queue.empty():
        fldr = folder_queue.get()

        print(fldr)
        if not os.path.exists(f"{repo_orig}/{fldr}/"):
            continue

        # find all files in orig/new with this folder name
        new_files = glob.glob(f"{repo_new}/{fldr}/*.*")
        orig_files = glob.glob(f"{repo_orig}/{fldr}/*.*")

        # put in correct relative format
        new_files = set([os.path.basename(i) for i in new_files])
        orig_files = set([os.path.basename(i) for i in orig_files])

        # take ones that are in common, diff them
        files_to_diff = list(new_files.intersection(orig_files))
        if not os.path.exists(f"{log_dir}/{fldr}/"):
            os.makedirs(f"{log_dir}/{fldr}/")
        for f in files_to_diff:
            orig = f"{repo_orig}/{fldr}/{f}"
            new  = f"{repo_new}/{fldr}/{f}"
            output = open(f"{log_dir}/{fldr}/diff_{f[:-2]}.txt", 'w')
            subprocess.call(["diff", orig, new], stdout=output)
            output.close()

        # if folders exist in the current common subfolder, 
        # add them to the list of folders to evaluate
        subfolders_new = glob.glob(f"{repo_new}/{fldr}/*/")
        for i in subfolders_new:
            folder_queue.put(f"{fldr}/{os.path.basename(os.path.dirname(i))}")


if __name__ == "__main__":
    form_diffs_for_repo("../../rfSLAM", "../../Downloads/randomForestSRC-2.6.1")