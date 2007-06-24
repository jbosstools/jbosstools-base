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
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URL;

import org.xml.sax.InputSource;

import org.jboss.tools.common.util.HttpUtil;
import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author igels
 */
public class KbDtdResource implements KbResource {

	private String uri;
	private URL url;
	private File dtdLocation;
	private File schemaLocation;
	private String rootElement;
	private InputSource inputSource;

	/**
	 * 
	 * @param uri
	 * @param inputSource
	 * @param rootElement
	 */
	public KbDtdResource(String uri, InputSource inputSource, String rootElement) {
		this.uri = uri;
		this.rootElement = rootElement;
		this.inputSource = inputSource;
	}

	/**
	 * 
	 * @param uri
	 * @param url
	 * @param rootElement
	 */
	public KbDtdResource(String uri, URL url, String rootElement) {
		this.uri = uri;
		this.url = url;
		this.rootElement = rootElement;
	}

	/**
	 * 
	 * @param uri
	 * @param dtdLocation
	 * @param rootElement
	 */
	public KbDtdResource(String uri, File dtdLocation, String rootElement) {
		this.uri = uri;
		this.dtdLocation = dtdLocation;
		this.rootElement = rootElement;
	}

	/**
	 * 
	 */
	public InputStream getInputStream() {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("--> KbDtdResource.getInputStream()");
			KbPlugin.log("    this resource = " + this);
		}

		InputStream is = null;

		try {
			if(dtdLocation != null) {
				is = new BufferedInputStream(new FileInputStream(dtdLocation));
				if(KbPlugin.isDebugEnabled()) {
					KbPlugin.log("    dtd location is not null and is will getting from dtd location");
				}
			} else if((url != null)) {
				if("jar".equals(url.getProtocol())) {
					is = url.openStream();
				} else if(KbConfigurationFactory.getInstance().getDefaultConfiguration().isAllowDownload()) {
					if(KbPlugin.isDebugEnabled()) {
						KbPlugin.log("    the dtd is will getting from url because url is not null and downloading from internet is alowed");
					}
					is = HttpUtil.getInputStreamFromUrlByGetMethod(url.toString());
				}
			} else if(inputSource != null) {
				String systemId = inputSource.getSystemId();
				String publicId = inputSource.getPublicId();
				String id;
				if(systemId!=null) {
					id = systemId;
				} else if(publicId!=null) {
					id = publicId;
				} else {
					if(KbPlugin.isDebugEnabled()) {
						KbPlugin.log("    system id and public id from input sourec are null and so can't to get url from input source");
						KbPlugin.log("<-- KbDtdResource.getInputStream()");
						KbPlugin.log("    return is = null");
					}
					return null;
				}

				is = HttpUtil.getInputStreamFromUrlByGetMethod(id);
			}
		} catch (Exception e) {
			KbPlugin.log(e);
		}

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("<-- KbDtdResource.getInputStream()");
			KbPlugin.log("    return is = " + is);
		}
		return is;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object ob) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("--> KbDtdResource.equals(Object ob)");
			KbPlugin.log("    this = " + this);
			KbPlugin.log("    ob = " + ob);
		}

		if(ob == this) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.log("<-- KbDtdResource.equals(Object ob)");
				KbPlugin.log("    return = true");
			}
			return true;
		}
		if((!(ob instanceof KbDtdResource)) || (ob == null)) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.log("<-- KbDtdResource.equals(Object ob)");
				KbPlugin.log("    return = false");
			}
			return false;
		}
		KbDtdResource resource = (KbDtdResource)ob;

		boolean eqUri = false;
//		boolean eqUrl = false;
//		boolean eqLocation = false;
//		boolean eqInputSource = false;
		boolean eqId = false;
		if(this.uri!=null) {
			eqUri = this.uri.equals(resource.getUri());
		} else if(resource.getUri()==null) {
			eqUri = true;
		}
		if(this.getId()!=null) {
			eqId = this.getId().equals(resource.getId());
		} else if(resource.getId()==null) {
			eqId = true;
		}
/*		if(this.url!=null) {
			eqUrl = this.url.equals(resource.getUrl());
		} else if(resource.getUrl()==null) {
			eqUrl = true;
		}
		if(this.dtdLocation!=null) {
			eqLocation = this.dtdLocation.equals(resource.getDtdLocation());
		} else if(resource.getDtdLocation()==null) {
			eqLocation = true;
		}
		if(this.inputSource!=null) {
			eqInputSource = this.inputSource.equals(resource.getInputSource());
		} else if(resource.getInputSource()==null) {
			eqInputSource = true;
		}
*/
		boolean result = eqUri||eqId;
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("<-- KbDtdResource.equals(Object ob)");
			KbPlugin.log("    return = " + result);
		}

		return result;
	}

	/**
	 * @return
	 */
	public File getDtdLocation() {
		return dtdLocation;
	}

	/**
	 * @return
	 */
	public String getUri() {
		return uri;
	}

	/**
	 * @return
	 */
	public URL getUrl() {
		return url;
	}

	/**
	 * 
	 * @return
	 */
	public String getId() {
		if(url!=null) {
			return url.toString();
		} else if((inputSource!=null)&&(inputSource.getPublicId()!=null)) {
				return inputSource.getPublicId();
		} else if((inputSource!=null)&&(inputSource.getSystemId()!=null)) {
			return inputSource.getSystemId();
		}
		return dtdLocation.toString();
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
		buffer.append("URI=\"");
		buffer.append(uri);
		buffer.append("\" URL=\"");		
		buffer.append(url);
		buffer.append("\" ID=\"");		
		buffer.append(getId());
		buffer.append("\" DTDlocation=\"");		
		buffer.append(dtdLocation);
		buffer.append("\" InputSource=\"");		
		buffer.append(inputSource);
		buffer.append("\" SchemaLocation=\"");		
		buffer.append(schemaLocation);
		buffer.append("\" RootElement=\"");		
		buffer.append(rootElement);
		buffer.append("\"");		

		return buffer.toString();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isModified() {
//		KbPlugin.log("--> KbDtdResource.isModified()");

		if(dtdLocation==null) {
			return false;
		}

		if(dtdLocation.exists()&&(schemaLocation!=null)&&(schemaLocation.exists())) {
			long lastModifiedDtd = dtdLocation.lastModified();
			long lastModifiedSchema = schemaLocation.lastModified();
//			KbPlugin.log("<-- KbTldResource.isModified()");
//			KbPlugin.log("    return: " + (lastModifiedSchema < lastModifiedDtd));
			return lastModifiedSchema < lastModifiedDtd;
		}

//		KbPlugin.log("<-- KbDtdResource.isModified()");
		return true;
	}

    /**
     * @return
     */
    public String getRootElement() {
        return rootElement;
    }

    /**
     * @param string
     */
    public void setRootElement(String name) {
        rootElement = name;
    }

    /**
     * @return
     */
    public InputSource getInputSource() {
        return inputSource;
    }
}