package org.jboss.tools.common.model.test;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.options.impl.XStudioDataLoaderImpl.PreferencesFileException;
import org.jboss.tools.common.model.plugin.ModelPlugin;

import junit.framework.TestCase;

public class PaletteLoaderTest extends TestCase {

	public static class ExceptionLogger implements ILogListener {

		private Set<IStatus> exceptions = new HashSet<IStatus>();

		public ExceptionLogger() {
			Platform.addLogListener(this);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.core.runtime.ILogListener#logging(org.eclipse.core.runtime.IStatus, java.lang.String)
		 */
		public void logging(IStatus status, String plugin) {
			if(status.getException() instanceof PreferencesFileException) {
				exceptions.add(status);
			}
		}

		public Set<IStatus> getExceptions() {
			return exceptions;
		}
	}

	protected volatile static ExceptionLogger LOGGER;

	public static ExceptionLogger initLogger() {
		LOGGER = new ExceptionLogger();
		return LOGGER;
	}

	public PaletteLoaderTest() {}

	public void testMessages() throws Exception {
		Set<IStatus> exceptions = LOGGER.getExceptions();
		assertFalse(exceptions.isEmpty());
		boolean testPaletteXMLIsReported = false;
		for (IStatus status: exceptions) {
			if(ModelPlugin.PLUGIN_ID.equals(status.getPlugin())) {
				PreferencesFileException exc = (PreferencesFileException)status.getException();
				String file = exc.getFile();
				if(file.endsWith("testpalette.xml")) {
					testPaletteXMLIsReported = true;
				} else {
					fail("Preference file " + file + " is reported as erroneous: " + exc.getXMLError());
				}
			}
		}
		assertTrue("Preference file testpalette.xml is not reported.", testPaletteXMLIsReported);
	}
}
