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
import java.util.List;
import java.util.StringTokenizer;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author eskimo
 */
public class JspKbConnector implements KbConnector {

	private ArrayList<KbResource> registretedResources;
	private ArrayList<KbDinamicResource> registretedDinamicResources;
//	private static KbResource jsfVariables = JsfValuesResource.getInstance();

	/**
	 * 
	 *
	 */
	public JspKbConnector() {
		registretedResources = new ArrayList<KbResource>();
		registretedDinamicResources = new ArrayList<KbDinamicResource>();
		KbResource jspResource = KbTldStore.getInstance().getJspResource();
		if(jspResource!=null) {
			registretedResources.add(jspResource);
		}
//		registerResource(jsfVariables);
	}

	/**
	 * @see org.jboss.tools.common.kb.KbConnector#queryTagInformation(org.jboss.tools.common.kb.KbQuery)
	 */
	public TagDescriptor getTagInformation(String query) throws KbException {
		KbPlugin.getPluginLog().logInfo("Query=" + query);
		String lastTag = getLastCompleteTag(query);
		KbPlugin.getPluginLog().logInfo("lastTag=" + lastTag);
		if(lastTag==null) {
			return null;
		}
		if(lastTag.indexOf(KbQuery.PREFIX_SEPARATOR)<0) {
			return KbHtmlStore.getInstance().queryTagInformation(new KbQuery(query, registretedResources));
		}
//		StringBuffer tldQuery = new StringBuffer();
//		tldQuery.append(KbQuery.TAG_SEPARATOR).append(lastTag).append(KbQuery.TAG_SEPARATOR);
//		return KbTldStore.getInstance().queryTagInformation(new KbQuery(tldQuery.toString(), registretedResources));
		return KbTldStore.getInstance().queryTagInformation(new KbQuery(query, registretedResources));
	}

	/**
	 * @see org.jboss.tools.common.kb.KbConnector#getProposals()
	 **/
	public Collection getProposals(String query) throws KbException {
		if(KbPlugin.isDebugEnabled()) { 
			KbPlugin.getPluginLog().logInfo("--> JspKbConnector.getProposals(String query)");
			KbPlugin.getPluginLog().logInfo("Query=" + query);
		} 

		int tagSeparator = query.indexOf(KbQuery.TAG_SEPARATOR);
		boolean jspDirectiveQuery = query.startsWith(KbQuery.JSP_DIRECTIVE_QUERY);
		if((tagSeparator<0)&&(!jspDirectiveQuery)) {
			// Bad query
			String errorMessage = "ERROR: Bad query: " + query + ". Query must starts with \"" + KbQuery.TAG_SEPARATOR + "\" or \"" + KbQuery.JSP_DIRECTIVE_QUERY + "\"";
			if(KbPlugin.isDebugEnabled()) { 
				KbPlugin.getPluginLog().logInfo(errorMessage);
			}
			throw new RuntimeException(errorMessage);
		}

		boolean autocompleteRequiredAttributes = KbConfigurationFactory.getInstance().getDefaultConfiguration().isAutocompleteRequiredAttributes(); 

		Collection<KbProposal> jspDirectiveProposals = new ArrayList<KbProposal>();
		if(query.startsWith(KbQuery.JSP_DIRECTIVE_QUERY)) {
			jspDirectiveProposals = KbJspDirectiveStore.getInstance().queryProposal(new KbQuery(query));
			if(!autocompleteRequiredAttributes) {
				removeAutocompleteRequiredAttributes(jspDirectiveProposals);
			}

			if(KbPlugin.isDebugEnabled()) { 
				KbPlugin.getPluginLog().logInfo("<-- JspKbConnector.getProposals(String query)");
				KbPlugin.getPluginLog().logInfo("    proposals size=" + jspDirectiveProposals.size());
			}

			return jspDirectiveProposals;
		} else if(query.lastIndexOf(KbQuery.TAG_SEPARATOR) + KbQuery.TAG_SEPARATOR.length() == query.length()) {
			jspDirectiveProposals = KbJspDirectiveStore.getInstance().queryProposal(new KbQuery(KbQuery.JSP_DIRECTIVE_QUERY));
		}

		JspQuery jspQuery = parseQuery(query);

		String tldQuery = jspQuery.getTldQuery();
		String htmlQuery = jspQuery.getHtmlQuery();

		if(KbPlugin.isDebugEnabled()) { 
			KbPlugin.getPluginLog().logInfo("tld query - " + tldQuery);
			KbPlugin.getPluginLog().logInfo("html query - " + htmlQuery);
		}

		Collection<KbProposal> tldProposals = new ArrayList<KbProposal>();
		Collection<KbProposal> htmlProposals = new ArrayList<KbProposal>();

		if(tldQuery!=null) {
			KbQuery kbQuery = new KbQuery(tldQuery, registretedResources, registretedDinamicResources);
			tldProposals = KbTldStore.getInstance().queryProposal(kbQuery);
		}

		if(jspQuery.getLastTldTag()!=null) {
			String mask = "";
			int lastSeparator = query.lastIndexOf(KbQuery.TAG_SEPARATOR);
			if((lastSeparator!=-1)&&(lastSeparator + KbQuery.TAG_SEPARATOR.length()<query.length())) {
				mask = query.substring(lastSeparator + KbQuery.TAG_SEPARATOR.length());
			}

			if(((mask.indexOf(KbQuery.DONT_FILTER_END_TAG_CHAR)!=-1)&&(KbQuery.DONT_FILTER_END_TAG_CHAR + jspQuery.getLastTldTag()).startsWith(mask))||(mask.equals(""))) {
				KbProposal proposal = new KbProposal();
				String label = "/" + jspQuery.getLastTldTag();
				proposal.setLabel(label);
				proposal.setReplacementString(label);
				proposal.setIcon(KbIcon.TLD_TAG);
				if(tldProposals instanceof List) {
					((List<KbProposal>)tldProposals).add(0, proposal);
				} else {
					tldProposals.add(proposal);
				}
			}
		}

		if(htmlQuery!=null) {
			boolean lowerCase = KbConfigurationFactory.getInstance().getDefaultConfiguration().isLowerCase();
			KbQuery kbQuery = new KbQuery(htmlQuery);
			htmlProposals = KbHtmlStore.getInstance().queryProposal(kbQuery);
			changeCase(htmlProposals, lowerCase);
		}

		ArrayList<KbProposal> proposals = mergeCollection(tldProposals, htmlProposals);
		proposals = mergeCollection(proposals, jspDirectiveProposals);
		proposals = sortProposals(proposals, query);

		if(!autocompleteRequiredAttributes) {
			removeAutocompleteRequiredAttributes(proposals);
		}

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- JspKbConnector.getProposals(String query)");
			KbPlugin.getPluginLog().logInfo("    proposals size=" + proposals.size());
		}

		return proposals;
	}

	private ArrayList<KbProposal> mergeCollection(Collection<KbProposal> col1, Collection<KbProposal> col2) {
		ArrayList<KbProposal> arrayList = new ArrayList<KbProposal>();
		Iterator<KbProposal> iterator = col1.iterator();
		while(iterator.hasNext()) {
			arrayList.add(iterator.next());
		}
		iterator = col2.iterator();
		while(iterator.hasNext()) {
			arrayList.add(iterator.next());
		}
		return arrayList;
	}

	private ArrayList<KbProposal> sortProposals(Collection<KbProposal> proposals, String query) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> JspKbConnector.sortProposals(Collection proposals)");
//			KbPlugin.getPluginLog().logInfo("Proposals = " + proposals);
		}

		ArrayList<KbProposal> closeTags = new ArrayList<KbProposal>();
		ArrayList<KbProposal> openTags = new ArrayList<KbProposal>(proposals.size());

		for(Iterator<KbProposal> iter = proposals.iterator(); iter.hasNext();) {
			KbProposal proposal = iter.next();
			if(proposal.isCloseTag()) {
				closeTags.add(proposal);
			} else {
				openTags.add(proposal);
			}
		}

		String lastTag = getLastCompleteTag(query);
		if((lastTag!=null)&&(closeTags.size()>0)) {
			for(Iterator iter = closeTags.iterator(); iter.hasNext();) {
				KbProposal proposal = (KbProposal)iter.next();
				if(proposal.getLabel().equals("/" + lastTag)) {
					closeTags.remove(proposal);
					closeTags.add(0, proposal);
					break;
				}
			}
		}

		ArrayList<KbProposal> collection = mergeCollection(closeTags, openTags);

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- JspKbConnector.sortProposals()");
//			KbPlugin.getPluginLog().logInfo("return Collection = [" + collection +"]");
		}

		return collection;
	}

	private JspQuery parseQuery(String query) {
		JspQuery jspQuery = new JspQuery();

		jspQuery.setLastTldTag(getLastTldTag(query));

		String lastTag = getLastTagMask(query);
		if(lastTag.indexOf(KbQuery.ATTRIBUTE_SEPARATOR)!=-1) {
			if(lastTag.indexOf(KbQuery.PREFIX_SEPARATOR)!=-1) {
				jspQuery.setTldQuery(KbQuery.TAG_SEPARATOR + lastTag);
				return jspQuery; 
			}
			jspQuery.setHtmlQuery(KbQuery.TAG_SEPARATOR + lastTag);
			return jspQuery;
		}
		jspQuery.setTldQuery(KbQuery.TAG_SEPARATOR + lastTag);

		if(lastTag.indexOf(KbQuery.PREFIX_SEPARATOR)!=-1) {
			return jspQuery;
		}

		StringTokenizer tags = new StringTokenizer(query, KbQuery.TAG_SEPARATOR, true);

		StringBuffer htmlQuery = new StringBuffer();
		while(tags.hasMoreTokens()) {
			String tag = tags.nextToken();
			if(!htmlQuery.toString().endsWith(KbQuery.TAG_SEPARATOR)) {
				htmlQuery.append(KbQuery.TAG_SEPARATOR);
			}
			if(tag.equals(KbQuery.TAG_SEPARATOR)) {
				continue;
			}
			if(tag.indexOf(KbQuery.PREFIX_SEPARATOR) < 0) {
				htmlQuery.append(tag);
			}
		}

		jspQuery.setHtmlQuery(htmlQuery.toString());

		return jspQuery;
	}

	private String getLastTldTag(String query) {
		StringTokenizer tags = new StringTokenizer(query, KbQuery.TAG_SEPARATOR, true);

		String lastTag = null;
		while(tags.hasMoreTokens()) {
			String tag = tags.nextToken();
			if(tag.equals(KbQuery.TAG_SEPARATOR)) {
				continue;
			}
			if((tag.indexOf(KbQuery.PREFIX_SEPARATOR) != -1)&&(tags.hasMoreTokens())) {
//				KbPlugin.log("tag - " + tag);
				lastTag = tag;
			}
		}
		return lastTag;
	}

	private String getLastTagMask(String query) {
		int lastTagSeparator = query.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if(lastTagSeparator < 0) {
			return null;
		}
		int endLastTagSeparator = lastTagSeparator + KbQuery.TAG_SEPARATOR.length();
		if(endLastTagSeparator < query.length()) {
			return query.substring(endLastTagSeparator);
		}
		return "";
	}

	private String getLastCompleteTag(String query) {
		int lastTagSeparator = query.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if(lastTagSeparator < 0) {
			return null;
		}

		String shortQuery = query.substring(0, lastTagSeparator);
		int nextToLastTagSeparator = shortQuery.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if(nextToLastTagSeparator < 0) {
			return null;
		}

		int endNextToLastTagSeparator = nextToLastTagSeparator + KbQuery.TAG_SEPARATOR.length();
		return query.substring(endNextToLastTagSeparator, lastTagSeparator);
	}

	/**
	 * @see org.jboss.tools.common.kb.KbConnector#RegisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public boolean registerResource(KbResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> JspKbConnector.registerResource(KbResource resource)");
			KbPlugin.getPluginLog().logInfo("    resource=" + resource);
		}
		if(resource instanceof KbTldResource) {
//			KbTldResource tldResource =(KbTldResource)resource;
			registretedResources.add(resource);
	
			KbTldStore.getInstance().registerResource(resource);
		} else if(resource instanceof KbDinamicResource) {
			registretedDinamicResources.add((KbDinamicResource)resource);
			KbTldStore.getInstance().registerResource(resource);
		} else {
			throw new RuntimeException("JspKbConnector.registerResource(KbResource resource): resource must be instance of KbTldResource or KbDinamicResource");
		}
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("    registretedResources=" + registretedResources);
			KbPlugin.getPluginLog().logInfo("<-- JspKbConnector.registerResource(KbResource resource)");
		}
		return true;
	}
/*
	private KbTldResource findEqualTldResource(KbTldResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("--> JspKbConnector.findEqualTldResource(KbTldResource resource)");
			KbPlugin.log("    reg size = " + registretedResources.size());
		}
		for(int i=0; i<registretedResources.size(); i++) {
//			KbPlugin.log(i + " = " + registretedResources.get(i));
			if(registretedResources.get(i).equals(resource)) {
				if(KbPlugin.isDebugEnabled()) {
					KbPlugin.log("<-- JspKbConnector.findEqualTldResource(KbTldResource resource)");
					KbPlugin.log("    return: " + registretedResources.get(i));
				}
				return (KbTldResource)registretedResources.get(i);
			}
		}

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("<-- JspKbConnector.findEqualTldResource(KbTldResource resource)");
			KbPlugin.log("    return: null");
		}
		return null;
	}
*/
	/**
	 * @see org.jboss.tools.common.kb.KbConnector#RegisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public void unregisterResource(KbResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> JspKbConnector.unregisterResource(KbResource resource)");
			KbPlugin.getPluginLog().logInfo("    resource: " + resource.toString());
		}

		if(resource instanceof KbTldResource) {
			KbTldResource tldResource =(KbTldResource)resource;

			KbTldStore.getInstance().unregisterResourcePrefix(tldResource);
			registretedResources.remove(resource);
		} else if(resource instanceof KbDinamicResource) {
			KbTldStore.getInstance().unregisterResource(resource);
			registretedDinamicResources.remove(resource);
		} else {
			throw new RuntimeException("JspKbConnector.unregisterResource(KbResource resource): resource must be instance of KbTldResource or KbDinamicResource");
		}

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- JspKbConnector.unregisterResource(KbResource resource)");
		}
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "Resources size = " + registretedResources.size();
	}

	private void changeCase(Collection proposals, boolean lowerCase) {
		for(Iterator iter = proposals.iterator(); iter.hasNext();) {
			((KbProposal)iter.next()).changeCase(lowerCase);
		}
	}

	private void removeAutocompleteRequiredAttributes(Collection proposals) {
		for(Iterator iter = proposals.iterator(); iter.hasNext();) {
			((KbProposal)iter.next()).removeAutocompleteRequiredAttributes();
		}
	}

	private class JspQuery {
		private String htmlQuery;
		private String tldQuery;
		private String lastTldTag;

		/**
		 * @return
		 */
		public String getHtmlQuery() {
			return htmlQuery;
		}

		/**
		 * @return
		 */
		public String getTldQuery() {
			return tldQuery;
		}

		/**
		 * @param string
		 */
		public void setHtmlQuery(String string) {
			htmlQuery = string;
		}

		/**
		 * @param string
		 */
		public void setTldQuery(String string) {
			if(string.indexOf(KbQuery.PREFIX_SEPARATOR + KbQuery.ATTRIBUTE_SEPARATOR) < 0) {
				tldQuery = string;
			}
		}

		/**
		 * @return
		 */
		public String getLastTldTag() {
			return lastTldTag;
		}

		/**
		 * @param string
		 */
		public void setLastTldTag(String string) {
			lastTldTag = string;
		}
	}
}