# Airline-management-Case-study
Airline Management System Case Study developed in COBOL on IBM Mainframe, demonstrating passenger booking, flight scheduling, ticket management, and record processing using VSAM/JCL concepts.
Airline Management System - COBOL Mainframe Case Study
Overview

This project is a case study of an Airline Management System developed using COBOL on an IBM Mainframe environment. The application simulates core airline operations such as flight management, passenger reservations, ticket booking, and reporting while demonstrating traditional mainframe development practices.

The project showcases the use of COBOL programming, batch processing, file handling, and JCL execution commonly used in enterprise airline systems.

Features
Flight Management
Add new flight records
Update flight schedules
View flight information
Search flights by route and flight number
Passenger Management
Register passenger details
Update passenger records
Maintain passenger database
Reservation & Ticket Booking
Book flight tickets
Cancel reservations
Check seat availability
Generate booking confirmations
Reporting
Passenger manifest reports
Flight occupancy reports
Daily booking summaries
Cancellation reports
Technology Stack
Language: COBOL
Platform: IBM Mainframe (z/OS)
Job Control: JCL (Job Control Language)
Data Storage: Sequential Files / VSAM Files
Utilities: SORT, IDCAMS
Development Environment: TSO/ISPF
Project Structure
AIRLINE-MANAGEMENT/
│
├── COBOL/
│   ├── FLIGHTM.cbl
│   ├── PASSENGERM.cbl
│   ├── BOOKINGM.cbl
│   └── REPORTM.cbl
│
├── JCL/
│   ├── COMPILE.jcl
│   ├── LINKEDIT.jcl
│   └── EXECUTE.jcl
│
├── COPYBOOKS/
│   ├── FLIGHTREC.cpy
│   ├── PASSREC.cpy
│   └── BOOKREC.cpy
│
├── DATA/
│   ├── FLIGHT.DAT
│   ├── PASSENGER.DAT
│   └── BOOKING.DAT
│
└── README.md
Business Scenario

An airline company requires a centralized system to manage:

Flight schedules
Passenger information
Ticket reservations
Seat allocation
Operational reports

The system processes airline data through batch jobs and maintains records efficiently using COBOL file processing techniques.

Learning Objectives

This case study demonstrates:

COBOL file handling
Mainframe batch processing
VSAM data management
Modular program design
JCL job execution
Enterprise application development concepts
Sample Workflow
Create flight records.
Register passengers.
Book flight reservations.
Update seat inventory.
Generate operational reports.
Execute batch jobs through JCL.
Future Enhancements
Online CICS transaction support
DB2 integration
Real-time seat allocation
Web service integration
Automated report scheduling
Author

Developed as a COBOL Mainframe Case Study for learning and demonstrating enterprise airline management processes on IBM Mainframe systems.
