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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;

import org.w3c.dom.Document;

/**
 * Describe TLD resource.
 * @author igels
 */
public class KbTldResource implements KbResource {

	private String uri;
	private String tldLocation;
	private File schemaLocation;
	private ArrayList<String> prefixes = new ArrayList<String>();
	private String tldContent;
	private String version;
	private boolean isCustomTld = false;
	private boolean jsfResource = false;

	/**
	 * Default Constructor.
	 */
	public KbTldResource() {
	}

	/**
	 * Constructor
	 * @param uri
	 * @param tldLocation
	 * @param prefix
	 */
	public KbTldResource(String uri, String tldLocation, String prefix, String version) {
		this.tldLocation = tldLocation;
		if(prefix!=null) {
			this.prefixes.add(prefix);
		}
		this.uri = uri;
		this.version = version;
	}

	/**
	 * 
	 * @return
	 */
	public String getTldContent() {
		return tldContent;
	}

	/**
	 * 
	 * @param tldContent
	 */
	public void setTldContent(String tldContent) {
		this.tldContent = tldContent;
	}

	/**
	 * Constructor
	 * @deprecated use KbTldResource(String uri, String tldLocation, String prefix)
	 */
	public KbTldResource(String tldLocation, String prefix) {
		this.tldLocation = tldLocation;
		if(prefix!=null) {
			this.prefixes.add(prefix);
		}
	}

	/**
	 * Return input stream of TLD.
	 */
	public InputStream getInputStream() {
		InputStream is = getInputStreamFromTldContent();
		if(is == null) {
			is = getInputStreamFromTldLocation();
		}
		return is;
	}

	private InputStream getInputStreamFromTldContent() {
        if (tldContent == null) {
        	return null;
        }
		return new ByteArrayInputStream(tldContent.getBytes());
	}

	private InputStream getInputStreamFromTldLocation() {
		InputStream is = null;
        if (tldLocation == null) {
        	return null;
        }
        try {
            if (tldLocation.indexOf(":/") < 2) {
                File file = new File(tldLocation);
                if (!file.exists()) {
                	return null;
                }
                is = new BufferedInputStream(new FileInputStream(file));
            } else {
				URL url = new URL(tldLocation);
				if("jar".equals(url.getProtocol()) || "file".equals(url.getProtocol())) {
					is = url.openStream(); // Bug 8385 -fixed
				}
            }
/*
            } else if(KbConfigurationFactory.getInstance().getDefaultConfiguration().isAllowDownload()) {// absolute url
				if(KbConfigurationFactory.getInstance().getDefaultConfiguration().isEnableProxy()) {
					if(KbPlugin.isDebugEnabled()) {
						KbPlugin.log("    try get Input Stream by HttpClient...");
					}
					is = KbHttpUtil.getInputStreamFromUrlByHttpClient(tldLocation);
				} else {
					if(KbPlugin.isDebugEnabled()) {
						KbPlugin.log("    try get Input Stream by java.net.URL(tldLocation).openStream()...");
					}
					is = new URL(tldLocation).openStream();
				}
            }
*/
		} catch (Exception e) {
            if(KbPlugin.isDebugEnabled()) {
                String message = "ERROR: TLD file (" + tldLocation + ") doesn't exist!";
    			KbPlugin.log(message, e);
            }
		}
		return is;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object ob) {
		if(ob == this) {
			return true;
		}
		if((!(ob instanceof KbTldResource)) || (ob == null)) {
			return false;
		}

		KbTldResource resource = (KbTldResource)ob;

		boolean eqUri = false;
		boolean eqLocation = false;
		boolean eqContent = false;
		boolean eqVersion = true;
		if(this.uri!=null) {
			eqUri = this.uri.equals(resource.getUri());
		} else if(resource.getUri()==null) {
			eqUri = true;
		}
		if(this.tldLocation!=null && this.tldLocation.length()>0) {
			eqLocation = this.tldLocation.equals(resource.getTldLocation());
		}
		if(this.tldContent!=null && this.tldContent.length()>0) {
			eqContent = this.tldContent.equals(resource.getTldContent());
		}
		if(this.version!=null && this.version.length()>0 && resource.getVersion()!=null && resource.getVersion().length()>0) {
			eqVersion = this.version.equals(resource.getVersion());
		}

		boolean result = (eqUri||eqLocation||eqContent) && eqVersion;

		return result;
	}

	/**
	 * 
	 * @param schema
	 * @return
	 */
	public boolean equalsTo(Document schema) {
		if(schema == null) {
			return false;
		}

		boolean eqUri = false;
		boolean eqLocation = false;
		String location = schema.getDocumentElement().getAttribute(SchemaNodeFactory.LOCATION_ATTRIBUTE);
		String uri = schema.getDocumentElement().getAttribute(SchemaNodeFactory.URI_ATTRIBUTE);
		if(this.uri!=null) {
			eqUri = this.uri.equals(uri);
		} else if(uri==null || uri.trim().length()==0) {
			eqUri = true;
		}
		if(this.tldLocation!=null && this.tldLocation.length()>0) {
			eqLocation = this.tldLocation.equals(location);
		}
//		else if(location==null || location.trim().length()==0) {
//			eqLocation = true;
//		}

		boolean result = eqUri||eqLocation;

		return result;
	}

	/**
	 * 
	 * @param newTldContent
	 * @return
	 */
	public boolean isModified(String newTldContent) {
		if(tldContent!=null && newTldContent!=null) {
			return !tldContent.equals(newTldContent);
		}
		return isModified();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isModified() {
		if((tldLocation!=null)&&(schemaLocation!=null)&&(schemaLocation.exists())) {
            long lastModifiedTld = 0;
            try {
				File file = new File(tldLocation);
				lastModifiedTld = file.lastModified();
            } catch(Exception e) {
            	return false;
            }
/*
            if (tldLocation.indexOf(":/") < 2) {
                File file = new File(tldLocation);
                lastModifiedTld = file.lastModified();
            } else {// absolute url
                try {
                    lastModifiedTld = new java.net.URL(tldLocation).openConnection().getLastModified();
                } catch (MalformedURLException e) {
    				KbPlugin.log(e);
                } catch (IOException e) {
    				KbPlugin.log(e);
                }
            }
*/
			long lastModifiedSchema = schemaLocation.lastModified();
			return lastModifiedSchema < lastModifiedTld;
		}
		return false;
	}

	/**
	 * 
	 * @param resource
	 * @return
	 */
	public boolean mergeTldLocation(KbTldResource resource) {
		String location = resource.getTldLocation();
		if(location==null || location.trim().length()==0) {
			return false;
		}
		if(this.tldLocation==null || this.tldLocation.trim().length()==0) {
			this.tldLocation = location;
			return true;
		}
		return false;
	}

	/**
	 * @return
	 */
	public String getTldLocation() {
		return tldLocation;
	}

	/**
	 * @return
	 */
	public File getSchemaLocation() {
		return schemaLocation;
	}

	/**
	 * @param file
	 */
	public void setSchemaLocation(File file) {
		schemaLocation = file;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("[TLDlocation=\"");
		buffer.append(tldLocation);
		buffer.append("\" SchemaLocation=\"");
		buffer.append(schemaLocation);
		buffer.append("\"");
		buffer.append(" Prefixes=\"");
		buffer.append(prefixes);
		buffer.append("\"");
		buffer.append(" uri=\"");
		buffer.append(uri);
		buffer.append("\"]");

		return buffer.toString();
	}

	/**
	 * @return
	 */
	public ArrayList<String> getPrefixes() {
		return prefixes;
	}

	/**
	 * @param prefixes
	 */
	public void setPrefixes(ArrayList<String> prefixes) {
		this.prefixes = prefixes;
	}

	/**
	 * @param string
	 */
	public boolean addPrefix(String prefix) {
	    for(int i=0; i<prefixes.size(); i++) {
	        if(prefixes.get(i).equals(prefix)) {
	            return false;
	        }
	    }
		return prefixes.add(prefix);
	}

	/**
	 * @param string
	 */
	public void removePrefix(String prefix) {
		for(Iterator iter = prefixes.iterator(); iter.hasNext();) {
			String pref = (String)iter.next();
			if(pref.equals(prefix)) {
				prefixes.remove(pref);
				return;
			}
		}
	}

	/**
	 * 
	 * @param prefix
	 * @return
	 */
	public boolean containsPrefix(String prefix) {
		for(Iterator iter = prefixes.iterator(); iter.hasNext();) {
			String pref = (String)iter.next();
			if(pref.equals(prefix)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public String getFirstPrefix() {
		if((prefixes==null)||(prefixes.size()==0)) {
			return null;
		}
		return (String)prefixes.get(0);
	}

    /**
     * @return
     */
    public String getUri() {
        return uri;
    }

    /**
     * 
     * @return
     */
	public String getId() {
		if(uri!=null && uri.trim().length()>0) {
			return uri;
		}
		return tldLocation;
	}

	/**
     * @see java.lang.Object#clone()
     */
    public Object clone() throws CloneNotSupportedException {
    	KbTldResource newResource = new KbTldResource(this.uri, this.tldLocation, null, this.version);
    	newResource.setTldContent(tldContent);
    	newResource.setCustomTld(isCustomTld());
    	newResource.prefixes = new ArrayList<String>();
    	newResource.prefixes.addAll(prefixes);
    	newResource.jsfResource = this.jsfResource;
        return newResource;
    }

    /**
     * 
     * @return
     */
	public String getVersion() {
		return version;
	}

	/**
	 * 
	 * @param version
	 */
	public void setVersion(String version) {
		this.version = version;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int hashCode = 0;
		if(uri!=null) {
			hashCode = uri.hashCode();
		} else if(tldContent!=null) {
			hashCode = tldContent.hashCode();
		} else if(tldLocation!=null) {
			hashCode = tldContent.hashCode();
		} else {
			hashCode = super.hashCode();
		}
		if(version!=null) {
			hashCode += version.hashCode();
		}
		return hashCode;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCustomTld() {
		return isCustomTld;
	}

	/**
	 * 
	 * @param isCustomTld
	 */
	public void setCustomTld(boolean isCustomTld) {
		this.isCustomTld = isCustomTld;
	}

	public boolean isJsfResource() {
		return jsfResource;
	}

	public void setJsfResource(boolean jsfResource) {
		this.jsfResource = jsfResource;
	}
}