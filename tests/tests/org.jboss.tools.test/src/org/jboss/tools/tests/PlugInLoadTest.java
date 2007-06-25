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
package org.jboss.tools.tests;

import junit.framework.TestCase;

import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class PlugInLoadTest extends TestCase {

	public static final String rhdsNS = "org.jboss.tools.";
	private static String jbideNS = "org.jboss.ide.eclipse.";
	private static String hibNS = "org.hibernate.eclipse.";
	private static String jbpmNS = "org.jbpm.gd.jpdl.";
	private static String jbwsNS = "com.eviware.soapui.";
	
	private boolean isPluginResolved (String pluginId)
	{
		Bundle bundle = Platform.getBundle(pluginId);
		
		assertNotNull(pluginId + " failed to load.", bundle);
		
		return ((bundle.getState() & Bundle.RESOLVED) > 0) ||
			((bundle.getState() & Bundle.ACTIVE) > 0);
	}
	
	private void assertPluginsResolved (String[] pluginIds)
	{
		for (int i = 0; i < pluginIds.length; i++) {
			assertTrue (isPluginResolved(pluginIds[i]));
		}
	}
	
	public void testCommonPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
				rhdsNS+"common",
				rhdsNS+"common.gef",
				rhdsNS+"common.kb",
				rhdsNS+"common.model",
				rhdsNS+"common.model.ui",
				rhdsNS+"common.projecttemplates",
				rhdsNS+"common.text.ext",
				rhdsNS+"common.text.xml",
				rhdsNS+"common.verification",
				rhdsNS+"common.verification.ui",
		});
	}
	
	public void testJsfPluginsResolved()
	{
		assertPluginsResolved(new String[] {
			rhdsNS+"jsf", 
			rhdsNS+"jsf.text.ext",
			rhdsNS+"jsf.text.ext.facelets",			
			rhdsNS+"jsf.ui",
			rhdsNS+"jsf.verification",
			rhdsNS+"jsf.vpe.ajax4jsf",
			rhdsNS+"jsf.vpe.facelets",
			rhdsNS+"jsf.vpe.otrix",
			rhdsNS+"jsf.vpe.richfaces",			
			rhdsNS+"jsf.vpe.seam",
			rhdsNS+"jsf.vpe.tomahawk"
		});
	}
	
	public void testJstPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			rhdsNS+"jst.jsp", 
			rhdsNS+"jst.server.jboss",
			rhdsNS+"jst.server.jetty",
			rhdsNS+"jst.server.jrun",
			rhdsNS+"jst.server.resin",
			rhdsNS+"jst.web",
			rhdsNS+"jst.web.debug",
			rhdsNS+"jst.web.debug.ui",
			rhdsNS+"jst.web.tiles",
			rhdsNS+"jst.web.tiles.ui",
			rhdsNS+"jst.web.ui",
			rhdsNS+"jst.web.verification"
		});
	}

	public void testVpePluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			rhdsNS + "vpe.mozilla",
			rhdsNS + "vpe.ui",
			rhdsNS + "vpe",
			rhdsNS + "vpe.ui.palette"
		});
	}	
	
	public void testStrutsPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			rhdsNS + "struts",
			rhdsNS + "struts.debug",
			rhdsNS + "struts.text.ext",
			rhdsNS + "struts.ui",
			rhdsNS + "struts.validator.ui",
			rhdsNS + "struts.verification"
		});
	}
	
	public void testShalePluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			rhdsNS + "shale.ui",
			rhdsNS + "shale",
			rhdsNS + "shale.text.ext"
		});
	}
	
	public void testCorePluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbideNS+"core", jbideNS+"jdt.core", jbideNS+"jdt.j2ee.core",
			jbideNS+"jdt.j2ee.ui", jbideNS+"jdt.j2ee.xml.ui", jbideNS+"jdt.test.core", jbideNS+"jdt.test.ui", jbideNS+"jdt.ui",
			jbideNS+"jdt.ws.core", jbideNS+"jdt.ws.ui", jbideNS+"archives.core", jbideNS+"archives.ui",
			jbideNS+"ui", jbideNS+"xdoclet.assist", jbideNS+"xdoclet.core", jbideNS+"xdoclet.run",
			jbideNS+"xdoclet.ui"
		});
	}
	
	public void testEjb3PluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbideNS+"ejb3.wizards.core", jbideNS+"ejb3.wizards.ui"
		});
	}
	
	public void testASPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbideNS+"as.core", jbideNS+"as.ui", jbideNS+"as.ui.mbeans"
		});
	}
	
	public void testHibernatePluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			"org.hibernate.eclipse", hibNS + "console", hibNS + "help", hibNS + "mapper", hibNS + "jdt.ui", hibNS + "jdt.apt.ui"
			
		});
	}
	
	public void testJbpmPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbpmNS + "core", jbpmNS + "ui"
		});
	}
	
	public void testFreemarkerPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbideNS+"freemarker"
		});
	}
	
	public void testDroolsPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			"org.drools.ide"
		});
	}
	
	public void testJBossWSPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbwsNS+"core", jbwsNS + "eclipse.core", jbwsNS+"jbosside.wstools", jbwsNS+"libs"
		});
	}
}
