/*******************************************************************************
 * Copyright (c) 2013-2018 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.stacks.core.test;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.jdf.stacks.model.Stacks;
import org.jboss.tools.stacks.core.model.StacksManager;
import org.junit.Test;
import junit.framework.TestCase;

public class StacksCoreTest extends TestCase {
	private static final String STACKS_URL_KEY = "org.jboss.examples.stacks.url";

	@Test
	public void testManagerFindsStacks() {
		Stacks s = new StacksManager().getStacks(new NullProgressMonitor());
		assertNotNull(s);
	}
	
	@Test
	public void testManagerUseDefaultStacks() {
		try {
			System.setProperty(STACKS_URL_KEY, "http://invalid.stacks.url");
			Stacks s = new StacksManager().getStacks(new NullProgressMonitor());
			assertNotNull(s);
		} finally {
			System.clearProperty(STACKS_URL_KEY);
		}
	}
	
}
