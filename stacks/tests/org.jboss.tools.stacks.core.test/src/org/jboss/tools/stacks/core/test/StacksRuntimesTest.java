/*******************************************************************************
  * Copyright (c) 2010 - 2018 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.stacks.core.test;

import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.jdf.stacks.model.Stacks;
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
			List<org.jboss.jdf.stacks.model.Runtime> rts = s.getAvailableRuntimes();
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
