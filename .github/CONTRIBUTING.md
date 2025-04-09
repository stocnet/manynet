# Contributing

Contributions to `manynet`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

## Aims

Here is some things that Guy Kawasaki, Silicon Valley venture capitalist,
learned from Steve Jobs:

- "Experts" are clueless. Especially self-declared ones.
- Customers cannot tell you what they need. They can help with evolution, but not revolution.
- Biggest challenges beget the best work.
- Design counts. Users will see the skin/UI of your product, not the great algorithms.
- Big graphics, big fonts.
- Jump curves---do things 10 times better, not 10 percent.
- All that truly matters is whether something works or doesn't work. Open or close, iPhone or Android, car or train, doesn't matter---make
it work.
- "Value" is different from "price". There is a class of people who do care about value. Ease of use -> less support costs. You have to create a unique and valuable product as an engineer.
- Real CEOs can demo. If you can't demo your own product, then quit.
- Real entrepreneurs ship, not slip.
- Some things need to be believed to be seen.

## Git and Bitbucket

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The Github page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

## Style

In terms of style, we are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.

## Fork

### Cloning
Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull 
This command allows you to `pull` changes from the remote repository to your local repository on Sourcetree.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose master or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 
Some documents might be stored using Large File Storage (LFS) to save space on the repository. 

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

## Issues and tests

Please use the issues tracker on Github to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.
Currently issue tracking is only open to those involved in the project.

The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

## Bug fixing or adding new code

Independent or assigned code contributions are most welcome.
When writing new code, please follow 
[standard R guidelines](https://www.r-bloggers.com/🖊-r-coding-style-guide/). 
It can help to use packages such as `lintr`, `goodpractice` and `formatR` 
to ensure these are followed.

Currently, commits can only be pushed to Bitbucket where they reference an existing issue.
If no issue exists for the code you have developed, please add an issue first before pushing.
Once the issue exists, you will need to mention the issue number (preceded by a hash symbol: #)
in the commit description:

``` Resolved #31 by adding a new function that does things, also updated documentation ```

Where the issue hash (i.e. #31) is preceded by
`resolve`, `resolves`, `resolved`, `close`, `closes`, `closed`, `fix`, `fixes`, or `fixed`
(capitalised or not),
Github will automatically updated the status of the issue(s) mentioned.

Our current syntactical standard is to mention the issue first and then 
provide a short description of what the committed changes do 
in relation to that issue.
Any ancillary changes can be mentioned after a comma.

## Documentation

A final way of contributing to the package is in developing the 
vignettes/articles that illustrate the value added in the package. 
Please contact me with any proposals here.

Please note that the `manynet` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

