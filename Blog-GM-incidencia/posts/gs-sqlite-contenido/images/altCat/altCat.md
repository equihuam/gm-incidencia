classDiagram
direction BT
class conceptos {
   text tema
   text concepto
   integer id_tema
   integer id
}
class drive_files {
   text _parent
   text _owner
   text lastModifyingUser
   text kind
   integer copyRequiresWriterPermission
   integer writersCanShare
   integer viewedByMe
   text mimeType
   text parents
   text iconLink
   integer shared
   text webViewLink
   integer viewersCanCopyContent
   integer hasThumbnail
   text spaces
   text name
   integer starred
   integer trashed
   integer explicitlyTrashed
   text createdTime
   text modifiedTime
   text quotaBytesUsed
   text version
   integer ownedByMe
   integer isAppAuthorized
   text thumbnailVersion
   integer modifiedByMe
   text linkShareMetadata
   text id
}
class drive_folders {
   text _parent
   text _owner
   text lastModifyingUser
   text kind
   text name
   text mimeType
   integer starred
   integer trashed
   integer explicitlyTrashed
   text spaces
   text version
   text webViewLink
   text iconLink
   integer hasThumbnail
   text thumbnailVersion
   integer viewedByMe
   text createdTime
   text modifiedTime
   integer modifiedByMe
   integer shared
   integer ownedByMe
   integer viewersCanCopyContent
   integer copyRequiresWriterPermission
   integer writersCanShare
   text folderColorRgb
   text quotaBytesUsed
   integer isAppAuthorized
   text linkShareMetadata
   text parents
   text id
}
class drive_users {
   text kind
   text displayName
   text photoLink
   integer me
   text emailAddress
   text permissionId
}
class tema_doc {
   text id_doc
   integer id_tema
}

conceptos  -->  conceptos : id_tema:id
drive_files  -->  drive_folders : _parent:id
drive_files  -->  drive_users : lastModifyingUser:permissionId
drive_files  -->  drive_users : _owner:permissionId
drive_folders  -->  drive_folders : _parent:id
drive_folders  -->  drive_users : _owner:permissionId
drive_folders  -->  drive_users : lastModifyingUser:permissionId
tema_doc  -->  conceptos : id_tema:id
tema_doc  -->  drive_files : id_doc:id
