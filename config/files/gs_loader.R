proj_name <- "irpdr"

## OUTSIDE Project
# Initialize and clone repo (using SSH)
system(paste0("gcloud init --account=652580964228-compute@developer.gserviceaccount.com --project=ml-learning-199501 && ",
              "gsutil cp -r gs://jmh_config/jmh_config/* . && ",
              "chmod 600 ~/.ssh/id_rsa && ",
              "chmod 600 ~/.ssh/id_rsa.pub && ",
              "git clone https://github.com/jasonmhoule/", proj_name, ".git"))

# Download & overwrite files from bucket (assumes existing folder)
system(paste0("gsutil -m cp -r gs://jmh/", proj_name, " ."))

## WITHIN Project
# For setup - change repo to SSH
system(paste0("git remote set-url origin git@github.com:jasonmhoule/", proj_name, ".git"))

# Resync files to bucket (creates folder if needed; ignores .git)
system(paste0("gsutil -m rsync -rd -x .git . gs://jmh/", proj_name))
