import setuptools

with open('README.md', 'r') as fh:
    long_description = fh.read()

setuptools.setup(
    name='lytspel',
    version='2.0.8',
    author='Christian Siefkes',
    author_email='christian@siefkes.net',
    description='A Simple Phonetic Respelling for the English Language',
    entry_points = {
        'console_scripts': ['lytspel=lytspel:main'],
    },
    long_description=long_description,
    long_description_content_type='text/markdown',
    license='ISC',
    url='https://www.lytspel.org/',
    packages=setuptools.find_packages(),
    install_requires=[
        'lxml >= 4.0.0',
        'setuptools >= 34.3.3',
        'spacy >= 2.0.0',
    ],
    # Keep minimum version listed here in sync with the one in INSTALL-USE.md
    python_requires='>=3.5',
    package_data={'lytspel': ['lytspel-dict.csv']},
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'License :: OSI Approved :: ISC License (ISCL)',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3',
        'Topic :: Communications',
        'Topic :: Text Processing :: Linguistic',
    ],
    keywords='english spelling reform lytspel',
    project_urls={
        'Source': 'https://github.com/ChristianSi/lytspel',
        'Tracker': 'https://github.com/ChristianSi/lytspel/issues',
    },
 )
