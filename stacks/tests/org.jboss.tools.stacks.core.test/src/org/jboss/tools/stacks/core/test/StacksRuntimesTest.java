package org.jboss.tools.stacks.core.test;

import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.developer.stacks.model.Stacks;
import org.jboss.tools.stacks.core.model.StacksManager;
import org.junit.Test;
import org.osgi.framework.Bundle;

public class StacksRuntimesTest extends TestCase {
	@Test
	public void testManagerFindRuntimes() {
		try {
			Bundle b = StacksCoreTestActivator.getContext().getBundle();
			URL stacksUrl = b.getEntry("data/pre-stacks.yaml");
			
			Stacks s = getProtectedManager().getStacks2(stacksUrl.toString(), "stacks", "yaml", new NullProgressMonitor());
			assertNotNull(s);
			List<org.jboss.developer.stacks.model.Runtime> rts = s.getAvailableRuntimes();
			assertEquals(11, rts.size());
		} catch(Throwable t) {
			throw new RuntimeException(t);
		}
	}
	private StacksManager2 getProtectedManager() {
		return new StacksManager2();
	}
	
	private static class StacksManager2 extends StacksManager {
		public Stacks getStacks2(String url, String prefix, String suffix, IProgressMonitor monitor) {
			return super.getStacks(url, "arbitraryString", monitor);
		}
	}
}
