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
package org.jboss.tools.common.kb.test;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.TestCase;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.kb.AttributeDescriptor;
import org.jboss.tools.common.kb.KbException;
import org.jboss.tools.common.kb.KbQuery;
import org.jboss.tools.common.kb.KbResource;
import org.jboss.tools.common.kb.KbTldResource;
import org.jboss.tools.common.kb.wtp.JspWtpKbConnector;
import org.jboss.tools.common.projecttemplates.ProjectTemplatesPlugin;
import org.osgi.framework.Bundle;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Tests RichFaces TLDs. This test loads TLD and try to get information of each tag and an attribute from KB. 
 * @author Alexey Kazakov
 */
public class TldTest extends TestCase {

	private static String A4J_TLD_PATH = "/RichFaces3.3/richfaces-ui-3.3.0.GA.jar!/META-INF/ajax4jsf.tld";
	private static String RICHFACES_TLD_PATH = "/RichFaces3.3/richfaces-ui-3.3.0.GA.jar!/META-INF/rich.tld";

	private JspWtpKbConnector connector = new JspWtpKbConnector();
	private String libFolderPath = null;

	public void testAjax4jsf() {
		testTld(A4J_TLD_PATH);
	}

	public void testRichFaces() {
		testTld(RICHFACES_TLD_PATH);
	}

	private void testTld(String tldPath) {
		String libFolder = getProjectTemplatesLibFolderLocation();
		String fullStringTldURL = "jar:file://" + libFolder + tldPath;
		URL tldLocation = null;
		try {
			tldLocation = new URL(fullStringTldURL);
		} catch (MalformedURLException e) {
			fail("Cannot load tld: " + fullStringTldURL + ". " + e.getMessage());
		}
		Document tld = loadTld(tldLocation);

		Element rootElement = tld.getDocumentElement();
		String version = getElementValue(rootElement, "tlib-version");
		String shortName = getElementValue(rootElement, "short-name");
		String uri = getElementValue(rootElement, "uri");

		KbResource a4jResource = new KbTldResource(uri, null, shortName, version);
		connector.registerResource(a4jResource, true);

		NodeList tags = rootElement.getElementsByTagName("tag");
		for(int i=0; i<tags.getLength(); i++) {
			Element tag = (Element)tags.item(i);
			String tagName = getElementValue(tag, "name");
			NodeList attributes = tag.getElementsByTagName("attribute");
			for(int j=0; j<attributes.getLength(); j++) {
				Element attribute = (Element)attributes.item(j);
				String attributeName = getElementValue(attribute, "name");
				String query = KbQuery.TAG_SEPARATOR + shortName + KbQuery.PREFIX_SEPARATOR + tagName + KbQuery.ATTRIBUTE_SEPARATOR + attributeName;
				try {
					AttributeDescriptor descriptor = connector.getAttributeInformation(query);
					assertNotNull("Cannot find any information about the attribute \"" + attributeName + "\" of the tag \"" + tagName + "\" in schema for tld \"" + uri + "\". Query = \"" + query + "\"", descriptor);
				} catch (KbException e) {
					fail("Cannot get any information by " + query + ". " + e.getMessage());
				}
			}
		}
	}

	private String getProjectTemplatesLibFolderLocation() {
		if(libFolderPath==null) {
			Bundle bundle = Platform.getBundle(ProjectTemplatesPlugin.PLUGIN_ID);
			try {
				libFolderPath = FileLocator.resolve(bundle.getEntry("/")).getPath() + "lib";
			} catch (IOException e) {
				fail("Cannot find lib folder (<org.jboss.tools.common.projecttemplates>/lib) with JARs to test TLDs");
			}
		}
		return libFolderPath;
	}

	private String getElementValue(Element parentElement, String elementName) {
		NodeList nl = parentElement.getElementsByTagName(elementName);
		Node node = nl.item(0);
		String value = node.getTextContent();
		return value.trim();
	}

	private Document loadTld(URL tldLocation) {
		InputStream is = null;
		try {
			is = tldLocation.openStream();
		} catch (IOException e) {
			fail("Cannot load tld: " + tldLocation + ". " + e.getMessage());
		}
		try {
			return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
		} catch (SAXException e) {
			fail("Cannot parse tld: " + tldLocation + ". " + e.getMessage());
		} catch (IOException e) {
			fail("Cannot parse tld: " + tldLocation + ". " + e.getMessage());
		} catch (ParserConfigurationException e) {
			fail("Cannot parse tld: " + tldLocation + ". " + e.getMessage());
		}
		return null;
	}
}