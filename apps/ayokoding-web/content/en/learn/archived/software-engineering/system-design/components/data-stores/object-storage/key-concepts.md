---
title: "Key Concepts"
weight: 703
date: 2025-03-16T07:20:00+07:00
draft: false
---

Object storage is a type of data storage architecture that manages data as objects rather than as blocks or files. In object storage, data is stored in a flat address space, and each object is assigned a unique identifier that can be used to retrieve it. Object storage is designed to handle large amounts of unstructured data, such as images, videos, and documents, and is commonly used in cloud computing environments.

## **Objects**

In object storage, data is stored as objects, which consist of the data itself, metadata that describes the object, and a unique identifier that can be used to retrieve the object. Objects can be of any size, from a few bytes to multiple terabytes, and can be accessed and modified independently of other objects.

For example, consider a photo-sharing application that allows users to upload and share photos. In a traditional file system, each photo would be stored as a separate file, and the file system would need to maintain a directory structure to organize the files. In an object storage system, each photo would be stored as an object, with metadata that describes the photo, such as the date it was taken, the location it was taken, and the user who uploaded it. The object storage system would assign a unique identifier to each photo, which could be used to retrieve the photo later.

## **Metadata**

Metadata is information that describes an object, such as its name, creation date, and file type. Metadata is stored with the object and can be used to search for and retrieve objects based on their attributes.

For example, consider a document management system that stores documents in an object storage system. Each document would be stored as an object, with metadata describing the document, such as the author, the date it was created, and the associated keywords. The object storage system would assign a unique identifier to each document, which could be used to retrieve the document later.

## **Flat Address Space**

Object storage data is stored in a flat address space, meaning objects are not organized in a hierarchical directory structure like traditional file systems. Instead, objects are stored in a single namespace, and each object is assigned a unique identifier that can be used to retrieve it.

For example, consider a video streaming service that stores videos in an object storage system. Each video would be stored as an object, with metadata that describes the video, such as the title, the length, and the resolution. The object storage system would assign a unique identifier to each video, which could be used to retrieve the video later.

## **Scalability**

Object storage is designed to be highly scalable, meaning it can handle large amounts of data and can be easily expanded as data storage needs grow. Object storage systems can be distributed across multiple servers and data centers, which allows for high availability and fault tolerance.

For example, consider a social media platform that stores user-generated content in an object storage system. As the platform grows and more users join, the amount of data that needs to be stored will increase. An object storage system can quickly expand to handle this growth by adding more servers or data centers.

## **Access Methods**

Object storage can be accessed using various methods, including RESTful APIs, file system gateways, and object storage gateways. RESTful APIs are commonly used in cloud computing environments, while file system gateways allow access to object storage using traditional file system protocols. Object storage gateways provide access to object storage using block storage protocols like iSCSI.

For example, consider a mobile application that stores user data in an object storage system. The application can use a RESTful API to access the object storage system, allowing users to upload and download data.

## **Examples**

Some examples of object storage systems include:

- Amazon S3
- Google Cloud Storage
- Microsoft Azure Blob Storage
- OpenStack Swift
- Ceph

## **Further Readings**

- [Object Storage Overview](https://www.ibm.com/cloud/learn/object-storage)
