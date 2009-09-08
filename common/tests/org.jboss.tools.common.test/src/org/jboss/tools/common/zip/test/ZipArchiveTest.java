/**
 * 
 */
package org.jboss.tools.common.zip.test;

import java.io.File;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.eclipse.core.internal.localstore.Bucket.Visitor;
import org.jboss.tools.common.zip.DefaultZipEntryVisitor;
import org.jboss.tools.common.zip.IZipEntryVisitor;
import org.jboss.tools.common.zip.UnzipOperation;
import org.jboss.tools.common.zip.ZipArchive;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class ZipArchiveTest extends ZipBaseTest {

	/**
	 * Test method for {@link org.jboss.tools.common.zip.ZipArchive#ZipArchive(java.lang.String)}.
	 */
	public void testZipArchiveString() {
		ZipArchive archive = new ZipArchive(getZip().getAbsolutePath());
		assertEquals(getZip().getAbsolutePath(),archive.getAbsolutePath());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.zip.ZipArchive#ZipArchive(java.io.File)}.
	 */
	public void testZipArchiveFile() {
		ZipArchive archive = new ZipArchive(getZip());
		assertEquals(getZip().getAbsolutePath(),archive.getAbsolutePath());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.zip.ZipArchive#acceptVisitor(org.jboss.tools.common.zip.IZipEntryVisitor)}.
	 */
	public void testAcceptVisitor() throws IOException{
		ZipArchive archive = new ZipArchive(getZip());
		ZipVisitor visitor = new ZipVisitor();
		archive.acceptVisitor(new UnzipOperation.FilteredZipEntryVisitor("META-INF.*", visitor));
		assertTrue(visitor.isDirVisited() && visitor.isFileVisited());
		
		ZipArchive.acceptVisitor(new ZipFileWrapper(getZip()),visitor);
	}
	
	public class ZipVisitor extends DefaultZipEntryVisitor {

		boolean dirVisited  = false;
		boolean fileVisited = false;
		
		public boolean isDirVisited() {
			return dirVisited;
		}

		public boolean isFileVisited() {
			return fileVisited;
		}
		
		@Override
		public void visiteDirectoryEntry(ZipFile zipFIle, ZipEntry dir)
				throws IOException {
			super.visiteDirectoryEntry(zipFIle, dir);
			this.dirVisited = true;
		}

		@Override
		public void visiteFileEntry(ZipFile zipFile, ZipEntry file)
				throws IOException {
			super.visiteFileEntry(zipFile, file);
			this.fileVisited = true;
		}
	}
	
	/**
	 * @author eskimo
	 *
	 */
	public class ZipFileWrapper extends ZipFile {

		public ZipFileWrapper(File file) throws ZipException, IOException {
			super(file);
		}

		@Override
		public void close() throws IOException {
			super.close();
			throw new IOException("Fake error");
		}
	}
}
