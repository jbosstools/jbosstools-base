package org.jboss.tools.common.zip;

import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public interface IZipEntryVisitor {
	void visiteDirectoryEntry (ZipFile zipFIle, ZipEntry dir) throws IOException;
	void visiteFileEntry (ZipFile zipFile, ZipEntry file) throws IOException;
}
