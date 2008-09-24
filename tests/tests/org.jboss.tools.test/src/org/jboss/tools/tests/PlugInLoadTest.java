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

	public static final String rhdsNS = "org.jboss.tools.";
	private static String jbideNS = "org.jboss.ide.eclipse.";
	private static String hibNS = "org.hibernate.eclipse.";
	private static String jbpmNS = "org.jbpm.gd.jpdl";
	private static String jbwsNS = "com.eviware.soapui.";

	private boolean isPluginResolved(String pluginId) {
		Bundle bundle = Platform.getBundle(pluginId);
		assertNotNull(pluginId + " failed to load.", bundle);
		try {
			// In 3.3 when test case is running plug-in.getState always returns
			// STARTING state
			// to move plug-in in ACTIVE state at  one class should be loaded
			// from plug-in
			bundle.loadClass("fake class");
		} catch (Exception e) {
			// It happens always because loaded class doesn't not exist
		}
		return ((bundle.getState() & Bundle.RESOLVED) > 0)
				|| ((bundle.getState() & Bundle.ACTIVE) > 0);
	}

	private void assertPluginsResolved(String[] pluginIds) {
		for (int i = 0; i < pluginIds.length; i++) {
			assertTrue("plugin '" + pluginIds[i] + "' is not resolved",
					isPluginResolved(pluginIds[i]));
		}
	}

	public void testCommonPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "common",
				rhdsNS + "common.gef", 
				rhdsNS + "common.kb",
				rhdsNS + "common.model", 
				rhdsNS + "common.model.ui",
				rhdsNS + "common.projecttemplates", 
				rhdsNS + "common.text.ext",
				rhdsNS + "common.text.xml", 
				rhdsNS + "common.verification",
				rhdsNS + "common.verification.ui", });
	}

	public void testJsfPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "jsf",
				rhdsNS + "jsf.text.ext", 
				rhdsNS + "jsf.text.ext.facelets",
				rhdsNS + "jsf.ui", 
				rhdsNS + "jsf.verification",
				rhdsNS + "jsf.vpe.ajax4jsf", 
				rhdsNS + "jsf.vpe.facelets",
				rhdsNS + "jsf.vpe.richfaces", 
				rhdsNS + "jsf.vpe.seam" });
	}

	public void testJstPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "jst.jsp",
				rhdsNS + "jst.web", 
				rhdsNS + "jst.web.debug",
				rhdsNS + "jst.web.debug.ui", 
				rhdsNS + "jst.web.tiles",
				rhdsNS + "jst.web.tiles.ui", 
				rhdsNS + "jst.web.ui",
				rhdsNS + "jst.web.verification" });
	}

	public void testVpePluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "vpe",
				rhdsNS + "vpe.ui.palette", 
				rhdsNS + "vpe.xulrunner" });
	}

	public void testStrutsPluginsResolved() {
		assertPluginsResolved(new String[] { 
				rhdsNS + "struts",
				rhdsNS + "struts.debug", 
				rhdsNS + "struts.text.ext",
				rhdsNS + "struts.ui", 
				rhdsNS + "struts.validator.ui",
				rhdsNS + "struts.verification" });
	}

	public void testCorePluginsResolved() {
		assertPluginsResolved(new String[] { 
				jbideNS + "archives.core",
				jbideNS + "archives.ui"});
	}

	public void testASPluginsResolved() {
		assertPluginsResolved(new String[] { 
				jbideNS + "as.core",
				jbideNS + "as.ui", 
				jbideNS + "as.ui.mbeans" });
	}

	public void testHibernatePluginsResolved() {
		assertPluginsResolved(new String[] { 
				"org.hibernate.eclipse",
				hibNS + "console", 
				hibNS + "help", 
				hibNS + "mapper",
				hibNS + "jdt.ui", 
				hibNS + "jdt.apt.ui" });
	}

	
	// TODO: Move this test to JBDS
//	public void testJbpmPluginsResolved() {
//		assertPluginsResolved(new String[] { 
//				jbpmNS });
//	}

	public void testFreemarkerPluginsResolved() {
		assertPluginsResolved(new String[] { 
				jbideNS + "freemarker" });
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
