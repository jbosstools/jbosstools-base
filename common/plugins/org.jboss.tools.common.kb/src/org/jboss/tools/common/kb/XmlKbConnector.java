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
package org.jboss.tools.common.kb;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author eskimo
 */
public class XmlKbConnector implements KbConnector {

	private ArrayList<KbResource> registretedResources;

	public XmlKbConnector() {
		registretedResources = new ArrayList<KbResource>();
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.kb.KbConnector#queryTagInformation(org.jboss.tools.common.kb.KbQuery)
	 */
	public TagDescriptor getTagInformation(String query) throws KbException {
		KbQuery kbQuery = new KbQuery(query, registretedResources);
		return KbDtdStore.getInstance().queryTagInformation(kbQuery);
	}

	/**
	 * @see org.jboss.tools.common.kb.KbConnector#getProposals()
	 **/
	public Collection getProposals(String query) throws KbException {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("--> XmlKbConnector.getProposals(String query)");
			KbPlugin.log("    query = " + query);
		}
/*
		try {
			KbPlugin.log("Query: " + query);
			KbPlugin.log("TagInfo: " + queryTagInformation(query));
		} catch(Exception e) {
    				KbPlugin.log(e);
		}
*/
		KbQuery kbQuery = new KbQuery(query, registretedResources);
		Collection proposals = KbDtdStore.getInstance().queryProposal(kbQuery);

		boolean autocompleteRequiredAttributes = KbConfigurationFactory.getInstance().getDefaultConfiguration().isAutocompleteRequiredAttributes();
		if(!autocompleteRequiredAttributes) {
			removeAutocompleteRequiredAttributes(proposals);
		}

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("<-- XmlKbConnector.getProposals(String query)");
			KbPlugin.log("    proposals size=" + proposals.size());
		}
		return proposals;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.kb.KbConnector#RegisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public boolean registerResource(KbResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("--> XmlKbConnector.registerResource(KbResource resource)");
			KbPlugin.log("    resource = " + resource);
		}

		if(!(resource instanceof KbDtdResource)) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.log("<-- XmlKbConnector.registerResource(KbResource resource)");
				KbPlugin.log("    resource don't instanse of KbDtdResource.");
			}
			return false;
		}
		KbDtdResource dtdResource =(KbDtdResource)resource;

		if(findEqualResource(resource)==null) {
			registretedResources.add(resource);
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.log("    resource has been registered.");
			}
			KbDtdStore.getInstance().registerResource(resource);
		}

		KbDtdStore.getInstance().reregisterModifiededResource(dtdResource);
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("<-- XmlKbConnector.registerResource(KbResource resource)");
		}
		return true;
	}

	private KbResource findEqualResource(KbResource resource) {
		for(int i=0; i<registretedResources.size(); i++) {
		 	if(registretedResources.get(i).equals(resource)) {
		 		return (KbResource)registretedResources.get(i);
		 	}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.kb.KbConnector#RegisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public void unregisterResource(KbResource resource) {
		KbResource er = findEqualResource(resource);

		if(er!=null) {
			registretedResources.remove(er);
		}
	}

	private void removeAutocompleteRequiredAttributes(Collection proposals) {
		for(Iterator iter = proposals.iterator(); iter.hasNext();) {
			((KbProposal)iter.next()).removeAutocompleteRequiredAttributes();
		}
	}

	public String toString() {
		return "Resources size = " + registretedResources.size();
	}
}