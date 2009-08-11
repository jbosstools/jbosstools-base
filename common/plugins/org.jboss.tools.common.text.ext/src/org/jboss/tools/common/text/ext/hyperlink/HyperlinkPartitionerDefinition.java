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
package org.jboss.tools.common.text.ext.hyperlink;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.custom.BusyIndicator;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.osgi.framework.Bundle;

/**
 * @author Igels
 */
public class HyperlinkPartitionerDefinition {

	private String fId = null;
	private String fClassName = null;

	// a hash map of content type Ids (String) that points to lists of parition types (List of Strings)
	// contentTypeId -> List(paritionType, paritionType, partitionType, ...)
	// contentTypeId2 -> List(partitionType, partitionType, ...)
	// ...
	private ContentTypeMap fContentTypes = null;

	private IConfigurationElement fConfigurationElement = null;

	/**
	 * @param id
	 * @param className
	 * @param configurationElement
	 */
    public HyperlinkPartitionerDefinition(String id, String className, IConfigurationElement configurationElement) {
		fId = id;
		fClassName = className;
		fConfigurationElement = configurationElement;
		fContentTypes = new ContentTypeMap();
    }

    /**
	 * @return IHyperlinkPartitioner for this definition
	 */
	public IHyperlinkPartitioner createHyperlinkPartitioner() {
	    IHyperlinkPartitioner hyperlinkPartitioner = null;

		if(getClassName()!=null) {
		    hyperlinkPartitioner = (IHyperlinkPartitioner)createExtension(HyperlinkPartitionerBuilder.ATT_CLASS);
		}

		return hyperlinkPartitioner;
	}

	public void addContentTypeId(String contentTypeId) {
        fContentTypes.addType(new ContentType(contentTypeId));
	}

	public void addPartitionType(String contentTypeId, String partitionTypeId) {
	    addContentTypeId(contentTypeId);
	    ContentType contentType = fContentTypes.getContentType(contentTypeId);
        contentType.addPartitionType(new PartitionType(partitionTypeId));
	}

	public void addAxis(String contentTypeId, String partitionTypeId, String axis, boolean ignoreCase) {
	    addPartitionType(contentTypeId, partitionTypeId);
	    PartitionType partitionType = fContentTypes.getContentType(contentTypeId).getPartitionType(partitionTypeId);
	    partitionType.addAxis(axis, ignoreCase);
	}

	/**
	 * @return Returns the fClass.
	 */
	public String getClassName() {
		return fClassName;
	}

	/**
	 * @return Returns the fConfigurationElement.
	 */
	public IConfigurationElement getConfigurationElement() {
		return fConfigurationElement;
	}

	/**
	 * @return Returns the fContentTypes.
	 */
	public List getContentTypes() {
		return fContentTypes.getTypes();
	}

	/**
	 * @return Returns the fId.
	 */
	public String getId() {
		return fId;
	}

	/**
	 * Creates an extension. If the extension plugin has not
	 * been loaded a busy cursor will be activated during the duration of
	 * the load.
	 * @param propertyName
	 * @return Object
	 */
	private Object createExtension(String propertyName) {
		// If plugin has been loaded create extension.
		// Otherwise, show busy cursor then create extension.
		final IConfigurationElement element = getConfigurationElement();
		final String name = propertyName;

		final Object[] result = new Object[1];
		Bundle bundle = Platform.getBundle(element.getDeclaringExtension().getNamespaceIdentifier()); 
		if (bundle.getState() == org.osgi.framework.Bundle.ACTIVE) {
			try {
				return element.createExecutableExtension(name);
			} catch (CoreException e) {
				handleCreateExecutableException(result, e);
			}
		} else {
			BusyIndicator.showWhile(null, new Runnable() {
				public void run() {
					try {
						result[0] = element.createExecutableExtension(name);
					} catch (CoreException e) {
						handleCreateExecutableException(result, e);
					}
				}
			});
		}
		return result[0];
	}

	/**
	 * @param result
	 * @param e
	 */
	private void handleCreateExecutableException(Object[] result, Exception x) {
		ExtensionsPlugin.getPluginLog().logError("Error in creating extension", x); //$NON-NLS-1$
		result[0] = null;
	}

	private static class TypeMap {

	    protected ArrayList<Object> fTypes =  new ArrayList<Object>();

	    public List getTypes() {
	        return fTypes;
	    }
	}

	private static class ContentTypeMap extends TypeMap {

	    public void addType(ContentType type) {
	        if(!fTypes.contains(type)) {
	            fTypes.add(type);
	        }
	    }

	    public ContentType getContentType(String id) {
	        for(int i=0; i<fTypes.size(); i++) {
	            ContentType type = (ContentType)fTypes.get(i);
	            if(type.getId().equals(id)) {
	                return type;
	            }
	        }
	        return null;
	    }
	}

	private static class PartitionTypeMap extends TypeMap {
	    public void addType(PartitionType type) {
	        if(!fTypes.contains(type)) {
	            fTypes.add(type);
	        }
	    }

	    public PartitionType getPartitionType(String id) {
	        for(int i=0; i<fTypes.size(); i++) {
	            PartitionType type = (PartitionType)fTypes.get(i);
	            if(type.getId().equals(id)) {
	                return type;
	            }
	        }
	        return null;
	    }
	}

	public static class ContentType {

	    private PartitionTypeMap fPartitionTypeMap;
	    private String fId;

	    public ContentType(String id) {
	        fId = id;
	        fPartitionTypeMap = new PartitionTypeMap();
	    }

	    public ContentType(String id, PartitionTypeMap partitionTypeMap) {
	        fId = id;
	        fPartitionTypeMap = partitionTypeMap;
	    }

	    public void addPartitionType(PartitionType type) {
	        if(!containsPartitiontype(type.getId())) {
	            fPartitionTypeMap.addType(type);
	        }
	    }

	    public List getPartitionTypes() {
	        return fPartitionTypeMap.getTypes();
	    }

	    public String getId() {
	        return fId;
	    }

	    public boolean containsPartitiontype(String id) {
	        return fPartitionTypeMap.getTypes().contains(new PartitionType(id));
	    }

	    public PartitionType getPartitionType(String id) {
	        return fPartitionTypeMap.getPartitionType(id);
	    }

        public boolean equals(Object obj) {
            if(obj == null || !(obj instanceof ContentType)) {
                return false;
            }
            ContentType type = (ContentType)obj;
            String objId = type.getId();
            return fId==null?objId==null:fId.equals(objId);
        }
	}

	public static class PartitionType {

	    private List<Axis> fAxises;
	    private String fId;

	    public PartitionType(String id) {
	        fId = id;
	        fAxises = new ArrayList<Axis>();
	    }

	    public void addAxis(String path, boolean ignoreCase) {
	        Axis axis = new Axis(path, ignoreCase);
	        for(int i=0; i<fAxises.size(); i++) {
	            if(axis.equals(fAxises.get(i))) {
	                return;
	            }
	        }
            fAxises.add(axis);
	    }

	    public List getAxises() {
	        return fAxises;
	    }

	    public String getId() {
	        return fId;
	    }

	    public boolean containtsAxis(String path) {
	        if(fAxises.size()==0 || path==null) {
	            return true;
	        }
	        for(int i=0; i<fAxises.size(); i++) {
	            Axis axis = (Axis)fAxises.get(i);
	            if(axis.containtsPath(path)) {
	                return true;
	            }
	        }
	        return false;
	    }

        public boolean equals(Object obj) {
            if(obj == null || !(obj instanceof PartitionType)) {
                return false;
            }
            PartitionType type = (PartitionType)obj;
            String objId = type.getId();
            return fId==null?objId==null:fId.equals(objId);
        }
	}

	public static class Axis {

	    private String fPath;
	    private boolean fIgnoreCase;
	    private String[] fDelimitedPathElements;
	    private boolean fStartDelim = false;
	    private boolean fEndDelim = false;

	    public Axis(String path, boolean ignoreCase) {
	        this.fIgnoreCase = ignoreCase;
	        setPath(path);
	    }

	    private String getCorrectPath(String path) {
	        path = path.trim();
	        if(fIgnoreCase) {
	            path = path.toLowerCase();
	        }
            if(!path.startsWith(DELIM)) {
                path = DELIM + path;
            }
            if(!path.endsWith(DELIM)) {
                path = path + DELIM;
            }
            return path;
	    }

	    public boolean isIgnoreCase() {
            return fIgnoreCase;
        }

	    public String getPath() {
            return fPath;
        }

	    public void setIgnoreCase(boolean ignoreCase) {
            this.fIgnoreCase = ignoreCase;
        }

	    public void setPath(String path) {
            this.fPath = getCorrectPath(path);

            if(fPath.indexOf(ALL_TAGS)>-1) {
    	        StringTokenizer st = new StringTokenizer(fPath, "*" , false); //$NON-NLS-1$
                fStartDelim = fPath.startsWith(ALL_TAGS);
                fEndDelim = fPath.endsWith(ALL_TAGS);
                ArrayList<String> list = new ArrayList<String>();
                while(st.hasMoreElements()) {
                    String element = (String)st.nextElement();
                    if(DELIM.equals(element)) {
                        continue;
                    }
                    list.add(element);
                }
                fDelimitedPathElements = (String[])list.toArray(new String[list.size()]);
            } else {
                fDelimitedPathElements = null;
            }
        }

	    private static String ALL_TAGS = "/*/"; //$NON-NLS-1$
	    private static String DELIM = "/"; //$NON-NLS-1$
	    private static String TAGLIB = "/["; //$NON-NLS-1$

	    public boolean containtsPath(String path) {
            path = getCorrectPath(path);
            if(ALL_TAGS.equals(fPath)) {
                return true;
            }
            if(fDelimitedPathElements!=null) {
                int lastIndex = -1;
                int tagLib = -1;
                for(int i=0; i<fDelimitedPathElements.length; i++) {
                    if(i==0 && !fStartDelim) {
                        if(path.startsWith(fDelimitedPathElements[i])) {
                            lastIndex = 0;
                            tagLib = 0;
                            continue;
                        } else {
                            return false;
                        }
                    } else {
                        int currentIndex = path.indexOf(fDelimitedPathElements[i]);
                        if(currentIndex>-1 && currentIndex>lastIndex) {
                            lastIndex = currentIndex;
                            if(fDelimitedPathElements[i].startsWith(TAGLIB))
                            	tagLib = lastIndex;
                            continue;
                        } else {
                            return false;
                        }
                    }
                }
                if(fEndDelim || path.endsWith(fDelimitedPathElements[fDelimitedPathElements.length-1])) {
                	int lastTaglib = path.lastIndexOf(TAGLIB);
                	if(lastTaglib <= tagLib)
                		return true;
                }
                return false;
            }

	        return fPath.equals(path);
	    }

	    public boolean equals(Object obj) {
            if(obj == null || !(obj instanceof Axis)) {
                return false;
            }
            Axis axis = (Axis)obj;
            String path = axis.getPath();
            boolean ignoreCase = axis.isIgnoreCase();
            return ((ignoreCase == fIgnoreCase) && fPath.equals(path));
        }
	}
}