/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.tests;

import junit.framework.TestCase;

import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;
/**
 * 
 * //TODO try to refactor it to use Eclipse API to get plug-ins list in installed feature
 * // 
 * 
 * @author eskimo
 *
 */
public class PlugInLoadTest extends TestCase {

	public static final String rhdsNS = "org.jboss.tools."; //$NON-NLS-1$
	private static String jbideNS = "org.jboss.ide.eclipse."; //$NON-NLS-1$
	private static String hibNS = "org.hibernate.eclipse."; //$NON-NLS-1$
	private static String jbpmNS = "org.jbpm.gd.jpdl"; //$NON-NLS-1$
	private static String jbwsNS = "com.eviware.soapui."; //$NON-NLS-1$

	private boolean isPluginResolved(String pluginId) {
		Bundle bundle = Platform.getBundle(pluginId);
		assertNotNull(pluginId + " failed to load.", bundle); //$NON-NLS-1$
		try {
			// In 3.3 when test case is running plug-in.getState always returns
			// STARTING state
			// to move plug-in in ACTIVE state at  one class should be loaded
			// from plug-in
			bundle.loadClass("fake class"); //$NON-NLS-1$
		} catch (Exception e) {
			// It happens always because loaded class doesn't not exist
		}
		return ((bundle.getState() & Bundle.RESOLVED) > 0)
				|| ((bundle.getState() & Bundle.ACTIVE) > 0);
	}

	private void assertPluginsResolved(String[] pluginIds) {
		for (int i = 0; i < pluginIds.length; i++) {
			assertTrue("plugin '" + pluginIds[i] + "' is not resolved", //$NON-NLS-1$ //$NON-NLS-2$
					isPluginResolved(pluginIds[i]));
		}
	}

	public void testCommonPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "common", //$NON-NLS-1$
				rhdsNS + "common.gef",  //$NON-NLS-1$
				rhdsNS + "common.kb", //$NON-NLS-1$
				rhdsNS + "common.model",  //$NON-NLS-1$
				rhdsNS + "common.model.ui", //$NON-NLS-1$
				rhdsNS + "common.projecttemplates",  //$NON-NLS-1$
				rhdsNS + "common.text.ext", //$NON-NLS-1$
				rhdsNS + "common.text.xml",  //$NON-NLS-1$
				rhdsNS + "common.verification", //$NON-NLS-1$
				rhdsNS + "common.verification.ui", }); //$NON-NLS-1$
	}

	public void testJsfPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "jsf", //$NON-NLS-1$
				rhdsNS + "jsf.text.ext",  //$NON-NLS-1$
				rhdsNS + "jsf.text.ext.facelets", //$NON-NLS-1$
				rhdsNS + "jsf.ui",  //$NON-NLS-1$
				rhdsNS + "jsf.verification", //$NON-NLS-1$
				rhdsNS + "jsf.vpe.ajax4jsf",  //$NON-NLS-1$
				rhdsNS + "jsf.vpe.facelets", //$NON-NLS-1$
				rhdsNS + "jsf.vpe.richfaces",  //$NON-NLS-1$
				rhdsNS + "jsf.vpe.seam" }); //$NON-NLS-1$
	}

	public void testJstPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "jst.jsp", //$NON-NLS-1$
				rhdsNS + "jst.web",  //$NON-NLS-1$
				rhdsNS + "jst.web.tiles", //$NON-NLS-1$
				rhdsNS + "jst.web.tiles.ui",  //$NON-NLS-1$
				rhdsNS + "jst.web.ui", //$NON-NLS-1$
				rhdsNS + "jst.web.verification" }); //$NON-NLS-1$
	}

	public void testVpePluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "vpe", //$NON-NLS-1$
				rhdsNS + "vpe.ui.palette",  //$NON-NLS-1$
				rhdsNS + "vpe.xulrunner" }); //$NON-NLS-1$
	}

	public void testStrutsPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "struts", //$NON-NLS-1$
				rhdsNS + "struts.text.ext", //$NON-NLS-1$
				rhdsNS + "struts.ui",  //$NON-NLS-1$
				rhdsNS + "struts.validator.ui", //$NON-NLS-1$
				rhdsNS + "struts.verification" }); //$NON-NLS-1$
	}

	public void testCorePluginsResolved() {
		assertPluginsResolved(new String[] { 
				jbideNS + "archives.core", //$NON-NLS-1$
				jbideNS + "archives.ui"}); //$NON-NLS-1$
	}

	public void testASPluginsResolved() {
		assertPluginsResolved(new String[] { 
				jbideNS + "as.core", //$NON-NLS-1$
				jbideNS + "as.ui",  //$NON-NLS-1$
				jbideNS + "as.ui.mbeans" }); //$NON-NLS-1$
	}

	public void testHibernatePluginsResolved() {
		assertPluginsResolved(new String[] { 
				"org.hibernate.eclipse", //$NON-NLS-1$
				hibNS + "console",  //$NON-NLS-1$
				hibNS + "help",  //$NON-NLS-1$
				hibNS + "mapper", //$NON-NLS-1$
				hibNS + "jdt.ui",  //$NON-NLS-1$
				hibNS + "jdt.apt.ui" }); //$NON-NLS-1$
	}

	
	// TODO: Move this test to JBDS
//	public void testJbpmPluginsResolved() {
//		assertPluginsResolved(new String[] { 
//				jbpmNS });
//	}

	public void testFreemarkerPluginsResolved() {
		assertPluginsResolved(new String[] { 
				jbideNS + "freemarker" }); //$NON-NLS-1$
	}

	public void testDroolsPluginsResolved() {
		// Skipped until drools migartion to 3.3 is finished
		// assertPluginsResolved(new String[] {
		// "org.drools.ide"
		// });
	}

	public void testJBossWSPluginsResolved() {
		// assertPluginsResolved(new String[] {
		// jbwsNS+"core",
		// jbwsNS+"eclipse.core",
		// jbwsNS+"jbosside.wstools",
		// jbwsNS+"libs"
		// });
	}

}
