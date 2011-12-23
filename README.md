# what it is

simp is a dead simple project defenition system that allows you to specify project properties and use them to do things like quick finding and grepping of project files.

# installing it

- first

    M-x package-install simp

- then, somewhere in your init

    (require 'simp)

# defining a project
A very simple generic project defenition to work with git projects:

    (simp-project-define
     '(:has (.git)
       :ignore (.git)))

A buffer will match as a member of the above simp project if it's associated
file has an ancestor directory that contains a .git file or directory, and that
when dealing with files in this project, the .git file or directory should be
excluded because you don't care about it.

You could likewise work with generic mercurial project like:

    (simp-project-define
     '(:has (.hg)
       :ignore (.hg)))

A more complex project defenition might look like this:

    (simp-project-define
     '(:type rails
       :has (config.ru app/views app/models app/controllers)
       :ignore (tmp coverage log vendor .git public/system public/assets)))

For a buffer to match as a member of this project, it must be a descendant of
some directory that contains each path in the :has list.

As above, the list of paths in the :ignore property will be excluded when
dealing with project files

In this example, ':type' is not actually going to be used by built-in simp
functions, but rather would be used to extend projects that match the ':has'
criteria.  Like all properties, a buffer's project's ':type' can be accessed
using (simp-project-get :type)

I use the following to work in my .emacs.d directory:

    (simp-project-define
     '(:type emacs
       :has (init.el)))

# how it works

When you attempt to take a project action in a buffer, simp looks at
the ancestor directories of the file associated with the buffer to
determine which project to use.  simp makes this determination by
verifying that some ancestor directory of the buffer's associated file
has all of the paths specified in a project defenitions ':has'
property.  Once it knows which project we are talking working with, it
is easy to scope actions to the project directory.
