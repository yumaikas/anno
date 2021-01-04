# Anno


Anno is a tool for scanning a directory laid out according to the Johnny decimal system.

It's written in Janet, so you'll need a janet install as of janet 1.13 or later.

Right now, it recognizes the following annotations to text files:

```
@todo[Description here | due: Date in YYYY-mm-dd format]
@appt[Description here | date: Date in YYYY-mm-dd format]
@til[Description here  | on: Date in YYYY-mm-dd format]
```

Example usage:


anno agenda
