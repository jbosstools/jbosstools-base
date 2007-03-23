/*
 * JBoss, a division of Red Hat
 * Copyright 2006, Red Hat Middleware, LLC, and individual contributors as indicated
 * by the @authors tag. See the copyright.txt in the distribution for a
 * full listing of individual contributors.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA, or see the FSF site: http://www.fsf.org.
 */
package org.jboss.ide.eclipse.core.test;

import junit.framework.TestCase;

import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class PluginLoadTest extends TestCase {

	private static String jbpkg = "org.jboss.ide.eclipse.";
	private static String hibpkg = "org.hibernate.eclipse.";
	private static String jbpmpkg = "org.jbpm.gd.jpdl.";
	private static String jbwspkg = "com.eviware.soapui.";
	
	private boolean isPluginResolved (String pluginId)
	{
		Bundle bundle = Platform.getBundle(pluginId);
		
		assertNotNull(pluginId + " failed to load.", bundle);
		
		return ((bundle.getState() & Bundle.RESOLVED) > 0);
	}
	
	private void assertPluginsResolved (String[] pluginIds)
	{
		for (int i = 0; i < pluginIds.length; i++) {
			assertTrue (isPluginResolved(pluginIds[i]));
		}
	}
	
	public void testCorePluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbpkg+"core", jbpkg+"jdt.core", jbpkg+"jdt.j2ee.core",
			jbpkg+"jdt.j2ee.ui", jbpkg+"jdt.j2ee.xml.ui", jbpkg+"jdt.test.core", jbpkg+"jdt.test.ui", jbpkg+"jdt.ui",
			jbpkg+"jdt.ws.core", jbpkg+"jdt.ws.ui", jbpkg+"packages.core", jbpkg+"packages.ui",
			jbpkg+"ui", jbpkg+"xdoclet.assist", jbpkg+"xdoclet.core", jbpkg+"xdoclet.run",
			jbpkg+"xdoclet.ui"
		});
	}
	
	public void testEjb3PluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbpkg+"ejb3.wizards.core", jbpkg+"ejb3.wizards.ui"
		});
	}
	
	public void testASPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbpkg+"as.core", jbpkg+"as.ui", jbpkg+"as.ui.mbeans"
		});
	}
	
	public void testHibernatePluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			"org.hibernate.eclipse", hibpkg + "console", hibpkg + "help", hibpkg + "mapper", hibpkg + "jdt.ui", hibpkg + "jdt.apt.ui"
			
		});
	}
	
	public void testJbpmPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbpmpkg + "help", jbpmpkg + "ui"
		});
	}
	
	public void testFreemarkerPluginsResolved ()
	{
		assertPluginsResolved(new String[] {
			jbpkg+"freemarker"
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
			jbwspkg+"core", jbwspkg + "eclipse.core", jbwspkg+"jbosside.wstools", jbwspkg+"libs"
		});
	}
}
