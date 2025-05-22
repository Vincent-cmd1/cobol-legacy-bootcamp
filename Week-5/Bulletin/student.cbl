      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. report.
       AUTHOR. AlexEnCode.

      ****************************************************************** 
      *    
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ****************************************************************** 
      *    
      ******************************************************************        
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ****************************************************************** 
      *    
      ****************************************************************** 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS. 

      *     SELECT F-OUTPUT
      *         ASSIGN TO 'output.dat'
      *         ACCESS MODE IS SEQUENTIAL
      *         ORGANIZATION IS LINE SEQUENTIAL
      *         FILE STATUS IS F-OUTPUT-STATUS.           

      ****************************************************************** 
      *    
      ****************************************************************** 
       DATA DIVISION.

      ****************************************************************** 
      *    
      ****************************************************************** 
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2       PIC 9(02).
       01  REC-F-INPUT-10      PIC X(10).
       01  REC-F-INPUT-100     PIC X(100).
       01  REC-F-INPUT-1000    PIC X(1000).

       01  REC-STUDENT.
           03 R-S-KEY          PIC 9(02).       
           03 R-LASTNAME       PIC X(07).       
           03 R-FIRSTNAME      PIC X(06).       
           03 R-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY          PIC 9(02).       
           03 R-LABEL          PIC X(21).       
           03 R-COEF           PIC 9V9.       
           03 R-GRADE          PIC 99V99.       

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT        PIC X(250).

      ****************************************************************** 
      *    
      ****************************************************************** 
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           05 STUDENT-LGHT PIC 9(03).
           05 COURSE-LGHT  PIC 9(03).
           05 STUDENT OCCURS 1 TO 999 TIMES DEPENDING ON STUDENT-LGHT.
               10 S-FIRSTNAME  PIC X(06).      
               10 S-LASTNAME   PIC X(07).
               10 S-AGE        PIC 9(02).      

      ****************************************************************** 
      *    
      ****************************************************************** 
       PROCEDURE DIVISION.


           STOP RUN.
