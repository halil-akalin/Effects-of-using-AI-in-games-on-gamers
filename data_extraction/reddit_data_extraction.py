import praw

"""
#Reddit api 
reddit = praw.Reddit(
    client_id = #reddit client id,
    client_secret = #reddit client secret,
    user_agent = #reddit user agent
)
"""

def pull_reddit_comments_and_create_txt_files(url, txt_file):
    submission = reddit.submission(url=url)

    submission.comments.replace_more(limit=None)
    comments = submission.comments.list()

    with open(txt_file, 'w', encoding='utf-8') as file:
        for comment in comments:
            file.write(comment.body + '\n')

pull_reddit_comments_and_create_txt_files(
    #reddit link, #txt file name to be created.txt)