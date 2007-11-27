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

import java.util.Collection;
import java.util.Iterator;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.jboss.tools.common.kb.KbDinamicResource;
import org.jboss.tools.common.kb.KbException;
import org.jboss.tools.common.kb.KbProposal;
import org.jboss.tools.common.kb.KbQuery;
import org.jboss.tools.common.kb.KbResource;
import org.jboss.tools.common.kb.KbTldResource;
import org.jboss.tools.common.kb.test.resources.TestBeanPropertyResource;
import org.jboss.tools.common.kb.test.resources.TestBundleNameResource;
import org.jboss.tools.common.kb.test.resources.TestBundlePropertyResource;
import org.jboss.tools.common.kb.test.resources.TestDynamicResource;
import org.jboss.tools.common.kb.test.resources.TestFaceletsJsfcResource;
import org.jboss.tools.common.kb.test.resources.TestFileResource;
import org.jboss.tools.common.kb.test.resources.TestJsfVariableResource;
import org.jboss.tools.common.kb.test.resources.TestTaglibResource;
import org.jboss.tools.common.kb.test.resources.TestViewActionResource;
import org.jboss.tools.common.kb.wtp.JspWtpKbConnector;

public class KbQueriesTest extends TestCase {

	private JspWtpKbConnector connector = new JspWtpKbConnector();

	public static Test suite() {
		return new TestSuite(KbQueriesTest.class);
	}

	public void testQueries() {
		KbResource jsfHtmlTld = new KbTldResource("http://java.sun.com/jsf/html", null, "h", "1.2");
		KbResource jsfCoreTld = new KbTldResource("http://java.sun.com/jsf/core", null, "f", "1.2");
		KbResource faceletsHtml = new KbTldResource("http://www.w3.org/1999/xhtml/facelets", null, "0fHP", null);
		connector.registerResource(jsfHtmlTld, true);
		connector.registerResource(jsfCoreTld, true);
		connector.registerResource(faceletsHtml, true);

		// Check tag name: <h:inputTex|
		String query = KbQuery.TAG_SEPARATOR + "h" + KbQuery.PREFIX_SEPARATOR + "inputTex";
		checkQuery(query, null, "h:inputText", "tag name");

		// Check attribute name <h:inputTex valu|
		query = KbQuery.TAG_SEPARATOR + "h" + KbQuery.PREFIX_SEPARATOR + "inputText" + KbQuery.ATTRIBUTE_SEPARATOR + "valu";
		checkQuery(query, null, "value", "attribute name");

		// Check list of bundles <f:loadBundle basename="|
		query = KbQuery.TAG_SEPARATOR + "f" + KbQuery.PREFIX_SEPARATOR + "loadBundle" + KbQuery.ATTRIBUTE_SEPARATOR + "basename" + KbQuery.ENUMERATION_SEPARATOR;	
		TestDynamicResource dynamicTestResource = new TestBundleNameResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "resource bundle");

		// Check list of bundle properties <h:inputText value="|
		query = KbQuery.TAG_SEPARATOR + "h" + KbQuery.PREFIX_SEPARATOR + "inputText" + KbQuery.ATTRIBUTE_SEPARATOR + "value" + KbQuery.ENUMERATION_SEPARATOR;	
		dynamicTestResource = new TestBundlePropertyResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "resource bundle property");

		// Check list of bean properties <h:inputText value="|
		dynamicTestResource = new TestBeanPropertyResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "bean properties");

		// Check list of JSF variables <h:inputText value="|
		dynamicTestResource = new TestJsfVariableResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "JSF variables");

		// Check list of actions <h:commandLink action="|
		query = KbQuery.TAG_SEPARATOR + "h" + KbQuery.PREFIX_SEPARATOR + "commandLink" + KbQuery.ATTRIBUTE_SEPARATOR + "action" + KbQuery.ENUMERATION_SEPARATOR;
		dynamicTestResource = new TestViewActionResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "actions");

		// Check list of files <h:commandButton image="|
		query = KbQuery.TAG_SEPARATOR + "h" + KbQuery.PREFIX_SEPARATOR + "commandButton" + KbQuery.ATTRIBUTE_SEPARATOR + "image" + KbQuery.ENUMERATION_SEPARATOR;
		dynamicTestResource = new TestFileResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "files");

		// Check list of TLDs <faceletsHtml:HTML xmlns="|
		query = KbQuery.TAG_SEPARATOR + "0fHP" + KbQuery.PREFIX_SEPARATOR + "HTML" + KbQuery.ATTRIBUTE_SEPARATOR + "xmlns" + KbQuery.ENUMERATION_SEPARATOR;
		dynamicTestResource = new TestTaglibResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "TLDs");

		// Check list for facelets attribute <0fHP:input jsfc="|
		query = KbQuery.TAG_SEPARATOR + "0fHP" + KbQuery.PREFIX_SEPARATOR + "input" + KbQuery.ATTRIBUTE_SEPARATOR + "jsfc" + KbQuery.ENUMERATION_SEPARATOR;
		dynamicTestResource = new TestFaceletsJsfcResource();
		checkQuery(query, dynamicTestResource, dynamicTestResource.getProposalLabels(), "facelets attribute jsfc");
	}

	private void checkQuery(String query, KbDinamicResource resource, String rightProposalLabel, String errorMessagePostfix) {
		checkQuery(query, resource, new String[]{rightProposalLabel}, errorMessagePostfix);
	}

	private void checkQuery(String query, KbDinamicResource resource, String[] rightProposalLabels, String errorMessagePostfix) {
		if(resource!=null) {
			connector.registerResource(resource, true);
		}
		Collection proposals = null;
		try {
			proposals = connector.getProposals(query);
		} catch (KbException e) {
			fail("Error getting proposals for " + errorMessagePostfix + ": " + e.getMessage());
		}
		String errorMessage = "Error getting proposals for " + errorMessagePostfix + ".";
		assertNotNull(errorMessage, proposals);
		if(proposals.isEmpty()) {
			fail(errorMessage);
		}
		assertTrue(errorMessage, proposalListContainsLabel(rightProposalLabels, proposals));
	}

	private boolean proposalListContainsLabel(String[] proposalLabels, Collection proposals) {
		Iterator iterator = proposals.iterator();
		while (iterator.hasNext()) {
			KbProposal proposal = (KbProposal) iterator.next();
			for(int i=0; i<proposalLabels.length; i++) {
				if(proposalLabels[i].equals(proposal.getLabel())) {
					return true;
				}
			}
		}
		return false;
	}
}