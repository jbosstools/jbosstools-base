package org.jboss.tools.runtime.test;

import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.internal.RuntimeCorePreferences;
import org.jboss.tools.runtime.core.internal.RuntimeExtensionManager;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osgi.service.prefs.BackingStoreException;

import junit.framework.TestCase;


/**
 * Test the initialization of the handlers
 * @author rob
 *
 */
public class RuntimeExtensionManagerTest  extends TestCase {
	public class TestExtension extends RuntimeExtensionManager {

		public Set<IRuntimeDetector> loadInitializedRuntimeDetectors() {
			return super.loadInitializedRuntimeDetectors();
		}
	}
	
	@Before
	public void setUp() {
		clearPrefs();
	}
	
	@After
	public void tearDown() {
		clearPrefs();
	}
	
	@Test
	public void testEnablementMatchesDefaults() {
		TestExtension test = new TestExtension();
		Set<IRuntimeDetector> initialized = test.loadInitializedRuntimeDetectors();
		Set<IRuntimeDetector> raw = test.loadDeclaredRuntimeDetectors();
		Iterator<IRuntimeDetector> rawIterator = raw.iterator();
		while(rawIterator.hasNext()) {
			IRuntimeDetector det = rawIterator.next();
			assertEquals(det.getId() + " does not match its default enablement", det.isEnabled(), getEnablement(det.getId(), initialized));
		}
	}
	
	private static final String HANDLER1 = "org.jboss.tools.runtime.handlers.TestHandler1";
	private static final String HANDLER2 = "org.jboss.tools.runtime.handlers.TestHandler2";
	
	
	private void setPrefs(String enabled, String disabled) {
		try {
			IEclipsePreferences prefs = ConfigurationScope.INSTANCE.getNode(RuntimeCoreActivator.PLUGIN_ID);
			prefs.put("enabledDetectors", enabled);
			prefs.put("disabledDetectors", disabled);
			prefs.flush();
		} catch(BackingStoreException bse) {
			bse.printStackTrace();
		}

	}
	
	public void testEnablementMatchesBothEnabledImplementation() {
		String handlers = HANDLER1 + "," + HANDLER2;
		setPrefs(handlers, "");
		
		TestExtension test = new TestExtension();
		Set<IRuntimeDetector> initialized = test.loadInitializedRuntimeDetectors();
		Iterator<IRuntimeDetector> i = initialized.iterator();
		while(i.hasNext()) {
			IRuntimeDetector rd = i.next();
			if( rd.getId().equals(HANDLER1) || rd.getId().equals(HANDLER2)) {
				assertTrue(rd.isEnabled());
			}
		}
	}
	
	public void testEnablementMatchesBothDisabledImplementation() {
		String handlers = HANDLER1 + "," + HANDLER2;
		setPrefs("", handlers);
		
		TestExtension test = new TestExtension();
		Set<IRuntimeDetector> initialized = test.loadInitializedRuntimeDetectors();
		Iterator<IRuntimeDetector> i = initialized.iterator();
		while(i.hasNext()) {
			IRuntimeDetector rd = i.next();
			if( rd.getId().equals(HANDLER1) || rd.getId().equals(HANDLER2)) {
				assertFalse(rd.isEnabled());
			}
		}
	}

	private void setEnabled(String id, boolean enabled, Set<IRuntimeDetector> det) {
		Iterator<IRuntimeDetector> i = det.iterator();
		while(i.hasNext()) {
			IRuntimeDetector d = i.next();
			if( d.getId().equals(id)) {
				d.setEnabled(enabled);
				return;
			}
		}
	}
	
	public void testEnablementMatchesBothEnabled() {

		TestExtension test = new TestExtension();
		Set<IRuntimeDetector> initialized = test.loadInitializedRuntimeDetectors();
		setEnabled(HANDLER1, true, initialized);
		setEnabled(HANDLER2, true, initialized);
		RuntimeCorePreferences.getDefault().saveDetectorEnablement(initialized);
		
		TestExtension test2 = new TestExtension();
		Set<IRuntimeDetector> initialized2 = test2.loadInitializedRuntimeDetectors();
		Iterator<IRuntimeDetector> i = initialized2.iterator();
		while(i.hasNext()) {
			IRuntimeDetector rd = i.next();
			if( rd.getId().equals(HANDLER1) || rd.getId().equals(HANDLER2)) {
				assertTrue(rd.isEnabled());
			}
		}
	}
	
	public void testEnablementMatchesBothDisabled() {
		TestExtension test = new TestExtension();
		Set<IRuntimeDetector> initialized = test.loadInitializedRuntimeDetectors();
		setEnabled(HANDLER1, false, initialized);
		setEnabled(HANDLER2, false, initialized);
		RuntimeCorePreferences.getDefault().saveDetectorEnablement(initialized);
		
		TestExtension test2 = new TestExtension();
		Set<IRuntimeDetector> initialized2 = test2.loadInitializedRuntimeDetectors();
		Iterator<IRuntimeDetector> i = initialized2.iterator();
		while(i.hasNext()) {
			IRuntimeDetector rd = i.next();
			if( rd.getId().equals(HANDLER1) || rd.getId().equals(HANDLER2)) {
				assertFalse(rd.isEnabled());
			}
		}
	}
	
	
	private void clearPrefs() {
		try {
			IEclipsePreferences prefs = ConfigurationScope.INSTANCE.getNode(RuntimeCoreActivator.PLUGIN_ID);
			prefs.put("enabledDetectors", "");
			prefs.put("disabledDetectors", "");
			prefs.flush();
			
		} catch(BackingStoreException bse) {
			bse.printStackTrace();
		}
	}
	
	private boolean getEnablement(String id, Set<IRuntimeDetector> set) {
		Iterator<IRuntimeDetector> initialized = set.iterator();
		while(initialized.hasNext()) {
			IRuntimeDetector i = initialized.next();
			if( i.getId().equals(id)) {
				return i.isEnabled();
			}
		}
		throw new RuntimeException();
	}
	
}
