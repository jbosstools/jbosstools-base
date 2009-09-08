package org.jboss.tools.common.zip;

import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class ZipArchive {

	File file = null;
	
	public ZipArchive(String file) {
		this(new File(file));
	}
	
	public ZipArchive(File file) {
		this.file = file;
	}

	public void acceptVisitor(IZipEntryVisitor visitor) throws IOException{
		ZipFile zipFile = null;
		zipFile = new ZipFile(this.file);
		acceptVisitor(zipFile, visitor);
	}
	
	public static void acceptVisitor(ZipFile zipFile, IZipEntryVisitor visitor) throws IOException {
		try {
			Enumeration<? extends ZipEntry> entries = zipFile.entries();
			while(entries.hasMoreElements()) {
				ZipEntry  entry = entries.nextElement();
				if(entry.isDirectory()) {
					visitor.visiteDirectoryEntry(zipFile, entry);
				} else {
					visitor.visiteFileEntry(zipFile, entry);
				}
			}
		} finally {
			if(zipFile!=null) {
				try {
					zipFile.close();
				} catch (IOException e) {
					// Nothing to do with that
				}
			}
		}
	}
	
	public String getAbsolutePath() {
		return file.getAbsolutePath();
	}
}
