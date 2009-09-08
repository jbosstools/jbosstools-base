package org.jboss.tools.common.zip;

import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class DefaultZipEntryVisitor implements IZipEntryVisitor {

	public void visiteDirectoryEntry(ZipFile zipFIle, ZipEntry dir) throws IOException {
	}

	public void visiteFileEntry(ZipFile zipFile, ZipEntry file) throws IOException {
	}
}
