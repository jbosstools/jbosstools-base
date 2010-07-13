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

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.MessageFormat;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.TestCase;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Abstract test is intended to test that all plug-ins of particular feature are loaded at run-time
 * @author eskimo
 *
 */
public class AbstractPluginsLoadTest extends AbstractRuntimeTest {


	private static final String PLUGIN_TAG_NAME = "plugin";
	private static final String ID_ATTRIBUTE_NAME = "id"; 

	public void testBundlesAreLoadedFor(final String featureId) {
		Bundle firstBundle = getFirstBundleFor(featureId);
		File bundleLocation = null;
		try {
			bundleLocation = FileLocator.getBundleFile(firstBundle);
		} catch (IOException e1) {
			fail(MessageFormat.format("Cannot find location for feature {0}",featureId));
		}
		File featuresFolder = new File(bundleLocation.getParentFile().getParentFile(),"features");
		String[] features = featuresFolder.list(new FilenameFilter() {
			
			public boolean accept(File dir, String name) {
				File featureDescriptor = new File(new File(dir,name),"feature.xml");
				return name.startsWith(featureId) && featureDescriptor.canRead();
			}
		});
		assertEquals(1, features.length);
		DocumentBuilder builder;
		File featureDescriptor = new File(new File(featuresFolder,features[0]),"feature.xml");
		try {
			builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			Document feature = builder.parse(featureDescriptor);
			NodeList plugins = feature.getElementsByTagName(PLUGIN_TAG_NAME);
			for (int i = 0; i < plugins.getLength(); i++) {
				String pluginId = plugins.item(i).getAttributes().getNamedItem(ID_ATTRIBUTE_NAME).getNodeValue();
				isPluginResolved(pluginId);
				System.out.println(MessageFormat.format("Plugn {0} was resolved and activated", pluginId));
			}
		} catch (ParserConfigurationException e) {
			fail(MessageFormat.format("Cannot configure parser to parse feature descriptor ''{0}''",featureDescriptor.getAbsolutePath()));
		} catch (SAXException e) {
			fail(MessageFormat.format("Cannot parse feature descriptor ''{0}''",featureDescriptor.getAbsolutePath()));
		} catch (IOException e) {
			fail(e.getMessage());
		}
	}

	public void assertPluginsResolved(Bundle[] bundles) {
		for (Bundle bundle : bundles) {
			assertTrue("Plugin '" + bundle.getSymbolicName() + "' is not resolved", //$NON-NLS-1$ //$NON-NLS-2$
					isPluginResolved(bundle.getSymbolicName()));
			System.out.println(bundle.getSymbolicName() + " was resolved and activated");
		}
	}
	
	public void assertPluginResolved(Bundle bundle) {
		assertPluginsResolved(new Bundle[] {bundle});
	}
	
	public void assertPluginsResolved(String[] ids) {
		for (String id : ids) {
			Bundle bundle = Platform.getBundle(id);
			assertNotNull(MessageFormat.format("Could not get bundle {0} instance",id), bundle);
			assertTrue(MessageFormat.format("Plugin '{0}' is not resolved",bundle.getSymbolicName()), //$NON-NLS-1$ //$NON-NLS-2$
					isPluginResolved(bundle.getSymbolicName()));
			System.out.println(MessageFormat.format("{0} was resolved and activated",bundle.getSymbolicName()));
		}
	}
	
	public void assertPluginResolved(String id) {
		assertPluginsResolved(new String[] {id});
	}
}
