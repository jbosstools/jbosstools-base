package org.jboss.tools.common.zip.test;

import java.io.File;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;

import junit.framework.TestCase;

public class ZipBaseTest extends TestCase {
	
	public static final String JAVA_IO_TMPDIR = "java.io.tmpdir";
	public static final String ORG_ECLIPSE_CORE_RUNTIME_ID = "org.eclipse.core.runtime";
	
	private File zip;
	private File temp;

	@Override
	protected void setUp() throws Exception {
		zip = FileLocator.getBundleFile(Platform.getBundle(ORG_ECLIPSE_CORE_RUNTIME_ID));
		temp = new File(System.getProperty(JAVA_IO_TMPDIR));
	}

	@Override
	protected void tearDown() throws Exception {
		zip = null;
		temp = null;
	}
	
	public File getZip() {
		return zip;
	}

	public File getTemp() {
		return temp;
	}	
}
