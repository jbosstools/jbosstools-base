/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.kb.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.kb.KbPlugin;
import org.osgi.framework.Bundle;

public class StartPluginTest extends TestCase {

	public static Test suite() {
		return new TestSuite(StartPluginTest.class);
	}

	public void testLoadingPlugin() {
		Bundle bundle = Platform.getBundle(KbPlugin.PLUGIN_ID);
		assertNotNull("Plug-in " + KbPlugin.PLUGIN_ID + " was not load", bundle);
	}
}