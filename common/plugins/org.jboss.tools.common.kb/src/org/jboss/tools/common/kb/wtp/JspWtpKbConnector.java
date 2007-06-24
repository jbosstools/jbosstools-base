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
package org.jboss.tools.common.kb.wtp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.jboss.tools.common.kb.AttributeDescriptor;
import org.jboss.tools.common.kb.KbDinamicResource;
import org.jboss.tools.common.kb.KbException;
import org.jboss.tools.common.kb.KbHtmlStore;
import org.jboss.tools.common.kb.KbProposal;
import org.jboss.tools.common.kb.KbQuery;
import org.jboss.tools.common.kb.KbResource;
import org.jboss.tools.common.kb.KbStore;
import org.jboss.tools.common.kb.KbTldResource;
import org.jboss.tools.common.kb.KbTldStore;
import org.jboss.tools.common.kb.TagDescriptor;

/**
 * Connector for JSP WTP Content assist processors.
 * @author Igels
 */
public class JspWtpKbConnector implements WtpKbConnector {

    private ArrayList<KbResource> registerResources;
	private ArrayList<KbDinamicResource> registretedDinamicResources;
    private KbResource jspResource;
//	private static KbResource jsfVariables = JsfValuesResource.getInstance();

	private Map<String,KbProposal> proposalCache = new HashMap<String,KbProposal>(500);
	private Map<String,Collection<KbProposal>> proposalsCacheForEmptyRequest = new HashMap<String,Collection<KbProposal>>(500);
	private Map<String,TagDescriptor> tagInfoCache = new HashMap<String,TagDescriptor>(500);
	private Map<String,AttributeDescriptor> attributeInfoCache = new HashMap<String,AttributeDescriptor>(1000);
	private Map<String,List<String>> tagNamesCache = new HashMap<String,List<String>>(1000);

	private void clearCache(boolean clearKbCache, boolean clearHtmlFaceletsCash) {
		proposalCache.clear();
		tagInfoCache.clear();
		attributeInfoCache.clear();
		proposalsCacheForEmptyRequest.clear();
		tagNamesCache.clear();
		if(clearHtmlFaceletsCash) {
			clearFaceletsHtmlCash();
		}
		if(clearKbCache) {
			KbTldStore.getInstance().clearCache();
		}
	}

	public void clearFaceletsHtmlCash() {
		KbTldStore.getInstance().clearFaceletsHtmlCash();
	}

	private Map<String,Collection<KbProposal>> getProposalsCacheForEmptyRequest() {
	    if(proposalsCacheForEmptyRequest.size()>1000) {
	    	proposalsCacheForEmptyRequest.clear();
	    }
	    return proposalsCacheForEmptyRequest;
	}
	
	private Map<String,KbProposal> getProposalCache() {
	    if(proposalCache.size()>1000) {
	        proposalCache.clear();
	    }
	    return proposalCache;
	}

	private Map<String,TagDescriptor> getTagInfoCache() {
	    if(tagInfoCache.size()>1000) {
	        tagInfoCache.clear();
	    }
	    return tagInfoCache;
	}

	private Map<String,AttributeDescriptor> getAttributeInfoCache() {
	    if(attributeInfoCache.size()>5000) {
	        attributeInfoCache.clear();
	    }
	    return attributeInfoCache;
	}

	private Map<String,List<String>> getTagNamesCache() {
	    if(tagNamesCache.size()>1000) {
	        tagNamesCache.clear();
	    }
	    return tagNamesCache;
	}

	/**
	 * Constructor.
	 */
	public JspWtpKbConnector() {
		jspResource = KbTldStore.getInstance().getJspResource();
		((KbTldResource)jspResource).addPrefix("jsp");
		registerResources = new ArrayList<KbResource>();
		registerJspResource();
		registretedDinamicResources = new ArrayList<KbDinamicResource>();
//		registerResource(jsfVariables);
	}

	/**
	 * Register JSP resource (<jsp:include...>, etc.)
	 */
	public void registerJspResource() {
		if(!registerResources.contains(jspResource)) {
			registerResources.add(jspResource);
		}
	}

	/**
	 * Unregister JSP resource (<jsp:include...>, etc.)
	 */
	public void unregisterJspResource() {
		registerResources.remove(jspResource);
	}

	/**
	 * Returns proposal from TLD Store.
	 * @param query
	 * @return
	 * @throws KbException
     * @see org.jboss.tools.common.kb.wtp.WtpKbConnector#getProposal(java.lang.String)
     */
    public KbProposal getProposal(String query) throws KbException {
    	if(!query.endsWith("/")) {
    		query = query + "/";
    	}
//    	KbPlugin.log(query);
        Object o = getProposalCache().get(query);
        if(o!=null) {
            return (KbProposal)o;
        }
		KbTldStore store = KbTldStore.getInstance();
		KbQuery kbQuery = new KbQuery(query, registerResources);
		Collection proposals = store.queryProposal(kbQuery);
		if(proposals.size()>0) {
		    Iterator iterator = proposals.iterator();
		    KbProposal p = (KbProposal)iterator.next();
		    getProposalCache().put(query, p);
		    return p;
		}
		return null;
    }

    /**
	 * @param query
	 * @return Tag Descriptor from TLD Store.
	 * @throws KbException
     * @see org.jboss.tools.common.kb.KbConnector#getTagInformation(java.lang.String)
     */
    public TagDescriptor getTagInformation(String query) throws KbException {
        Object o = getTagInfoCache().get(query);
        if(o!=null) {
            return (TagDescriptor)o;
        }

		KbQuery kbQuery = new KbQuery(query, registerResources, registretedDinamicResources);
		KbStore store = null;
        if(query.indexOf(':')<0) {
        	// Html query
            store = KbHtmlStore.getInstance();
        } else {
        	// TLD query
            store = KbTldStore.getInstance();
        }
        TagDescriptor tagInformation = store.queryTagInformation(kbQuery);
		getTagInfoCache().put(query, tagInformation);
		return tagInformation;
    }

    /**
	 * @param query
	 * @return Attribute Descriptor from TLD Store.
	 * @throws KbException
     * @see org.jboss.tools.common.kb.WtpKbConnector#getAttributeInformation(java.lang.String)
     */
    public AttributeDescriptor getAttributeInformation(String query) throws KbException {
        Object o = getAttributeInfoCache().get(query);
        if(o!=null) {
            return (AttributeDescriptor)o;
        }

		KbQuery kbQuery = new KbQuery(query, registerResources, registretedDinamicResources);
		KbStore store = null;
        if(query.indexOf(':')<0) {
        	// Html query
            store = KbHtmlStore.getInstance();
        } else {
        	// TLD query
            store = KbTldStore.getInstance();
        }
		AttributeDescriptor attributeInformation = store.queryAttributeInformation(kbQuery);
		getAttributeInfoCache().put(query, attributeInformation);
		return attributeInformation;
    }

	/**
	 * @param query
	 * @return Collection of proposals from TLD Store.
	 * @throws KbException
	 * @see org.jboss.tools.common.kb.KbConnector#getProposals()
	 */
	public Collection getProposals(String query) throws KbException {
		boolean emptyQuery = false;
		if(query.equals("/")) {
			emptyQuery = true;
		}
		if(emptyQuery) {
	        Object o = getProposalsCacheForEmptyRequest().get(query);
	        if(o!=null) {
	            return (Collection)o;
	        }
		}

		KbTldStore store = KbTldStore.getInstance();
		KbQuery kbQuery = new KbQuery(query, registerResources, registretedDinamicResources);
		Collection<KbProposal> proposals = store.queryProposal(kbQuery);
//		Collection result = removeSameProposals(proposals);
		if(emptyQuery) {
			getProposalsCacheForEmptyRequest().put(query, proposals);
		}
		return proposals;
	}

	/**
	 * Registers resource
	 * @param resource
	 * @see org.jboss.tools.common.kb.KbConnector#registerResource(org.jboss.tools.common.kb.KbResource)
	 */
	public boolean registerResource(KbResource resource) {
		return registerResource(resource, false);
	}

	/**
	 * Registers resource
	 * @param resource
	 * @param waitForRegistration
	 */
	public boolean registerResource(KbResource resource, boolean waitForRegistration) {
		boolean clearCash = false;
		boolean modified = false;
		boolean newResource = true;
		boolean registrated = true;
		if(resource instanceof KbTldResource) {
			if(((KbTldResource)resource).getFirstPrefix()!=null && ((KbTldResource)resource).getFirstPrefix().length()>0) {
				for (Iterator iter = registerResources.iterator(); iter.hasNext();) {
				    KbTldResource tldResource = (KbTldResource) iter.next();
			        if(resource.equals(tldResource)) {
			        	((KbTldResource)resource).setCustomTld(((KbTldResource)tldResource).isCustomTld());
			        	newResource = false;
			            String prefix = ((KbTldResource)resource).getFirstPrefix();
			            if(prefix != null) {
				            if(tldResource.addPrefix(prefix)) {
				            	clearCash = true;
				            }
				            if(tldResource.isModified(((KbTldResource)resource).getTldContent())) {
				            	KbResource newTldResource = (KbTldResource)KbTldStore.getInstance().registerResource(resource, waitForRegistration);
								((KbTldResource)tldResource).setCustomTld(((KbTldResource)newTldResource).isCustomTld());
								((KbTldResource)tldResource).setTldContent(((KbTldResource)newTldResource).getTldContent());
								((KbTldResource)tldResource).setJsfResource(((KbTldResource)newTldResource).isJsfResource());
				            	clearCash = true;
				            	modified = true;
				            }
			            }
			            break;
			        }
		        }
				if(newResource) {
	//				KbTldResource tldResource =(KbTldResource)resource;
					KbResource newTldResource = KbTldStore.getInstance().registerResource(resource, waitForRegistration);
					if(newTldResource!=null) {
						((KbTldResource)resource).setCustomTld(((KbTldResource)newTldResource).isCustomTld());
						((KbTldResource)resource).setTldContent(((KbTldResource)newTldResource).getTldContent());
						((KbTldResource)resource).setJsfResource(((KbTldResource)newTldResource).isJsfResource());
						registerResources.add(resource);
					} else {
						registrated = false;
					}
				}
			}
		} else if(resource instanceof KbDinamicResource) {
		    registretedDinamicResources.add((KbDinamicResource)resource);
		    KbTldStore.getInstance().registerResource(resource);
		} else {
		    throw new RuntimeException("JspWtpKbConnector.registerResource(KbResource resource): resource must be instance of KbTldResource or KbDinamicResource");
		}
		if(clearCash) {
			clearCache(modified, false);
		}
		return registrated;
	}

	/**
	 * @param uri
	 * @return
	 * @throws KbException
	 * @see org.jboss.tools.common.kb.wtp.WtpKbConnector#getAllTagNamesFromTldByUri(java.lang.String)
	 */
	public List getAllTagNamesFromTldByUri(String uri, String version) {
		String key = uri;
		if(version!=null) {
			key += version;
		}
        Object o = getTagNamesCache().get(key);
        if(o!=null) {
            return (List)o;
        }

	    KbTldStore store = KbTldStore.getInstance();
	    KbTldResource resource = new KbTldResource(uri, "", "", version);
	    List<String> result = store.getAllTagNamesFromResource(resource);
	    getTagNamesCache().put(key, result);
	    return result;
	}

	/**
	 * Unregisters resource
	 * @param resource 
	 * @see org.jboss.tools.common.kb.KbConnector#unregisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public void unregisterResource(KbResource resource) {
		clearCache(false, false);
		if(resource instanceof KbTldResource) {
			KbTldResource tldForStoreResource =(KbTldResource)resource;
			KbTldStore.getInstance().unregisterResource(tldForStoreResource);
			for (Iterator iter = registerResources.iterator(); iter.hasNext();) {
			    KbTldResource tldResource = (KbTldResource) iter.next();
		        if(resource.equals(tldResource)) {
		        	tldResource.removePrefix(tldForStoreResource.getFirstPrefix());
					if(tldResource.getPrefixes().isEmpty()) {
						registerResources.remove(tldResource);
					}
					break;
		        }
	        }
		} else if(resource instanceof KbDinamicResource) {
			KbTldStore.getInstance().unregisterResource(resource);
			registretedDinamicResources.remove(resource);
		} else {
			throw new RuntimeException("JspKbConnector.unregisterResource(KbResource resource): resource must be instance of KbTldResource or KbDinamicResource");
		}
	}

	public void unregisterAllResources() {
		unregisterAllResources(false);
	}

	/**
	 * @param clearKbCache Clear all cashe but don't clear Facelets HTML Resource
	 */
	public void unregisterAllResources(boolean clearKbCache) {
		clearCache(clearKbCache, false);
		registerResources.clear();
	}

/*
	private Collection<KbProposal> removeSameProposals(Collection proposals) {
	    ArrayList<KbProposal> list = new ArrayList<KbProposal>();
	    int i=0;
	    for (Iterator iter = proposals.iterator(); iter.hasNext();) {
	        i++;
            KbProposal proposal = (KbProposal) iter.next();
            if(!containsProposal(list, proposal)) {
                list.add(proposal);
            }
        }
	    return list;
	}

	private boolean containsProposal(ArrayList proposals, KbProposal proposal) {
	    for (Iterator iter = proposals.iterator(); iter.hasNext();) {
            if(proposal.equals(iter.next())) {
                return true;
            }
        }
	    return false;
	}
*/

}