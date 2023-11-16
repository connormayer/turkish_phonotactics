# This script takes a list of stimuli in IPA format and synthesizes it
# using Amazon Polly
from boto3 import Session
from botocore.exceptions import BotoCoreError, ClientError
from contextlib import closing
import csv
import os
import pandas as pd
import re
import sys
import subprocess
from tempfile import gettempdir

# Create a client using the credentials and region defined in the [adminuser]
# section of the AWS credentials file (~/.aws/credentials).
session = Session(profile_name="default")
polly = session.client("polly")

file = "data/turkish_phonotactic_judgments/candidates_ortho_v3.csv"
with open(file) as f:
    df = pd.read_csv(file)

filenames = []

for _, row in df.iterrows():
    word = ''.join((row['word'].split(' ')))
    print(word)
    try:
        # Request speech synthesis
        response = polly.synthesize_speech(
            Engine = 'standard',
            LanguageCode = 'tr-TR',
            Text="<speak><phoneme alphabet='ipa' ph='{}'>Blah</phoneme></speak>".format(word), 
            OutputFormat="mp3",
            VoiceId="Filiz",
            TextType="ssml"
        )
    except (BotoCoreError, ClientError) as error:
        # The service returned an error, exit gracefully
        print(error)
        sys.exit(-1)

    # Access the audio stream from the response
    if "AudioStream" in response:
        # Note: Closing the stream is important because the service throttles on the
        # number of parallel connections. Here we are using contextlib.closing to
        # ensure the close method of the stream object will be called automatically
        # at the end of the with statement's scope.
        with closing(response["AudioStream"]) as stream:
            root_file = "{}.mp3".format("_".join(row['word'].split(" ")))
            output = os.path.join('audio', root_file)

            try:
            # Open a file for writing the output as a binary stream
                with open(output, "wb") as file:
                   file.write(stream.read())
            except IOError as error:
                # Could not write to file, exit gracefully
                print(error)
                sys.exit(-1)
            filenames.append(root_file)
    else:
        # The response didn't contain audio data, exit gracefully
        print("Could not stream audio")
        sys.exit(-1)

df['filename'] = filenames
df.to_csv('data/turkish_phonotactic_judgments/stimuli_with_filenames.csv')