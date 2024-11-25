<!--

Thanks for reporting a bug.

If you fill out every section I can try to help.  If you don’t I’ll end up copy / pasting these questions again because I need them to at least try and reproduce the issue.

If your issue doesn’t fit this template, just be aware that the first thing I have to do is reproduce your issue.  I can’t do any debugging until I can reproduce the bug.

-->

<!-- 1/7 -->

There is an app that works in nixpkgs, but not when using mac-app-util. Its name is: ...

It is not broken in nixpkgs proper, because I can `nix build` it successfully:

<!-- 2/7 -->
```
$ nix --experimental-features 'nix-command flakes' build github:nixos/nixpkgs/nixpkgs-unstable#some-app
$ echo $?
0
```

And I can launch it:

<!-- 3/7 -->
```
$ open result/Applications/Some.app
```

But when I install the app using `mac-app-util`, and do:


<!-- 4/7: please select which one you used: -->

```
$ open ~/Applications/Home\ Manager\ Trampolines/Some.app
```

<!-- 4/7 OR: -->

```
$ open /Applications/Nix\ Trampolines/Some.app
```

<!-- 5/7 And substitute the app name 👆 -->

It crashes with error:

<!-- 6/7 -->
```
....
```

My system:

<!-- 7/7 -->
```
$ nix-info -m
....
```

- [ ] I have substituted **7** sections in this template.
- [ ] I have tested this with the `nixpkgs-unstable` branch and the problem persists
