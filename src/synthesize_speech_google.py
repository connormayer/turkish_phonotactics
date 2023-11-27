"""Synthesizes speech from the input string of text or ssml.
Make sure to be working in a virtual environment.

Note: ssml must be well-formed according to:
    https://www.w3.org/TR/speech-synthesis/
"""
from google.cloud import texttospeech
from google.oauth2 import service_account
import pandas as pd
import os
import re

# credentials = service_account.Credentials.from_service_account_file(
#     '/home/connor/.config/gcloud/configurations/weighty-gasket-366920-c347807d64db.json'
# )

client = texttospeech.TextToSpeechClient()

file = "data/turkish_phonotactic_judgments/grouped_stimuli.csv"
with open(file) as f:
    df = pd.read_csv(file)

filenames = []

for _, row in df.iterrows():
    with_stress = row['word'].split(' ')
    # All words are CVCVC, so stress on final is 3rd position
    with_stress.insert(2, 'ˈ')
    word = ''.join(with_stress)
    word = re.sub('ɫ', 'l', word)
    print(word)

    # Set the text input to be synthesized
    synthesis_input = texttospeech.SynthesisInput(
        ssml="<speak><phoneme alphabet='ipa' ph='{}'>Blah</phoneme></speak>".format(word)
    )

    # Build the voice request, select the language code ("en-US") and the ssml
    # voice gender ("neutral")
    voice = texttospeech.VoiceSelectionParams(
        language_code="tr-TR", name="tr-TR-Wavenet-A"
    )

    # Select the type of audio file you want returned
    audio_config = texttospeech.AudioConfig(
        audio_encoding=texttospeech.AudioEncoding.MP3
    )

    # Perform the text-to-speech request on the text input with the selected
    # voice parameters and audio file type
    response = client.synthesize_speech(input=synthesis_input, voice=voice, audio_config=audio_config)

    # The response's audio_content is binary.
    root_file = "{}.mp3".format(row['ortho'])
    output = os.path.join('audio', root_file)

    with open(output, "wb") as out:
        # Write the response to the output file.
        out.write(response.audio_content)
        print('Audio content written to file "{}"'.format(root_file))
    filenames.append(root_file)

df['filename'] = filenames
df.to_csv('data/turkish_phonotactic_judgments/stimuli_with_filenames.csv')