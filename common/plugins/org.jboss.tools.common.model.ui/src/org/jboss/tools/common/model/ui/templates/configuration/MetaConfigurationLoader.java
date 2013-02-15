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
package org.jboss.tools.common.model.ui.templates.configuration;

import java.io.*;
import java.net.URL;
import java.util.*;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.*;
import org.osgi.framework.Bundle;
import org.w3c.dom.*;
import org.jboss.tools.common.xml.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.templates.model.*;

public class MetaConfigurationLoader implements MetaTemplateConstants {
	static MetaConfigurationLoader instance = new MetaConfigurationLoader();
	
	static {
        try {
        	String path = FileLocator.resolve(MetaConfigurationLoader.class.getResource("/dtds/meta-templates.dtd")).toString(); //$NON-NLS-1$
            XMLEntityResolver.registerPublicEntity("-//Red Hat Inc.//DTD Meta Templates 1.0//EN", path); //$NON-NLS-1$
        } catch (IOException e) {
        	ModelUIPlugin.getPluginLog().logError(e);
        }
	}
    
	private static final String GLOBAL_FILE_NAME = "/globalTemplates.xml"; //$NON-NLS-1$
    private static final String PROJECT_FILE_NAME = "/.projectTemplates"; //$NON-NLS-1$
	
	public static final String PREFERENCE_KEY = "global_templates"; //$NON-NLS-1$
	
	public void loadExtensionConfiguration(MetaConfiguration c) {
		processExtensions(c);
	}

	public void loadGlobalConfiguration(MetaConfiguration c) {
		String text = ModelUIPlugin.getDefault().getPluginPreferences().getString(PREFERENCE_KEY);
		if(text != null && text.length() > 0) {
			Document document = XMLUtilities.getDocument(new StringReader (text), createResolver());		
			loadConfiguration(c, document);
		}
	}
	
	public void loadProjectConfiguration(MetaConfiguration c, IProject project) {
		Document document = getDocument(new Path(getProjectLocation(project)));
		loadConfiguration(c, document);
	}	

	public void saveGlobalConfiguration(MetaConfiguration c) {
		Element element = XMLUtilities.createDocumentElement(META_TEMPLATE_GROUPS);
		saveConfiguration(c, element);
		StringWriter writer = new StringWriter();
		try {
			XMLUtilities.serialize(element, writer);			
		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		ModelUIPlugin.getDefault().getPluginPreferences().setValue(PREFERENCE_KEY, writer.toString());				
	}
	
	public void saveProjectConfiguration(MetaConfiguration c, IProject project) {
		saveConfiguration(c, getProjectLocation(project));
	}

	void saveConfiguration(MetaConfiguration c, String location) {
		File f = new File(location);
		if(c.isOverriding()) {
			Element element = XMLUtilities.createDocumentElement(META_TEMPLATE_GROUPS);
			saveConfiguration(c, element);
			try {
				XMLUtilities.serialize(element, location);
			} catch (IOException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		} else if(f.isFile()) {
				f.delete();
		}
	}
	
	void loadConfiguration(MetaConfiguration c, Document document) {
	    Element[] groups = XMLUtilities.getChildren(document.getDocumentElement(), META_TEMPLATE_GROUP);
	    for (int i = 0;i < groups.length;++i) {
			MetaGroup g = c.addGroup(groups[i].getAttribute(URI));
			loadGroup(g, groups[i], null);
		}
	}

	protected String getGlobalLocation() {
		String location = Platform.getStateLocation(Platform.getBundle(ModelUIPlugin.ID_PLUGIN)).toString();
		return location + GLOBAL_FILE_NAME; 
	}

	protected String getProjectLocation(IProject project) {
		return project.getLocation().toString() + PROJECT_FILE_NAME;
	}
    
	// extensions 
    private void processExtensions(MetaConfiguration c) {
    	IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
    	if(extensionPoint == null) {
    		ModelUIPlugin.getPluginLog().logError("Cannot find extension point \"" + EXTENSION_POINT); //$NON-NLS-1$
    	}
    	IConfigurationElement[] members = extensionPoint.getConfigurationElements();
    	for (int m = 0; m < members.length; m++) {
    		IConfigurationElement member = members[m];
    		IExtension extension = member.getDeclaringExtension();
    		String name = extension.getNamespaceIdentifier();
   			if (INCLUDE.equals(member.getName())) {
   				doInclude(member, c);
   			} else if (META_TEMPLATE_GROUP.equals(member.getName())) {
   				doMetaTemplate(member, c);
   			} else {
   				ModelUIPlugin.getPluginLog().logInfo("Error in declaring extension \"" + EXTENSION_POINT+"\" at " + name); //$NON-NLS-1$ //$NON-NLS-2$
    		}
        }
    }
    
	private void doMetaTemplate(IConfigurationElement element, MetaConfiguration c) {
		MetaGroup g = c.addGroup(element.getAttribute(URI));
		loadGroup(g, element, null);
	}

	private void doInclude(IConfigurationElement element, MetaConfiguration c) {
		URL url = null;
		String name = element.getDeclaringExtension().getNamespaceIdentifier();
		Bundle bundle = Platform.getBundle(name);
		try {
			url = FileLocator.resolve(bundle.getEntry("/")); //$NON-NLS-1$
		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		String file = element.getAttribute(FILE);
		String translations = element.getAttribute(TRANSLATIONS);
		
		try {
			String fullPath = FileLocator.resolve(url).getFile();
			ResourceBundle properties = null;
			if (translations!=null && translations.length()>0) {
				properties = getResourceBundle(new Path(fullPath+translations));
			}
			
			Document document = getDocument(new Path(fullPath + file));
			if (document!=null) {
				Element root = document.getDocumentElement();
				MetaGroup g = c.addGroup(root.getAttribute(URI));
				loadGroup(g, root, properties);
			}
		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

	private ResourceBundle getResourceBundle(IPath path) {
		ResourceBundle properties = null;
		File propertiesFile = path.toFile();
		if (propertiesFile.canRead()) {
			InputStream is = null;
			try {
				is = new FileInputStream(propertiesFile);
				properties = new PropertyResourceBundle(is);
			} catch (IOException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} finally {
				if (is != null) {
					try {
						is.close();
					} catch (IOException e) {
						// ignore
					}
				}
				properties = new ResourceBundle() {
					@Override
					protected Object handleGetObject(String key) {
						return null;
					}
					@Override
					public Enumeration<String> getKeys() {
						return Collections.enumeration(Collections.<String>emptyList());
					}
				};
			}
		}
		return properties;
	}
	
	Document getDocument(IPath path) {
		File file = new File(path.toString());
		if(!file.isFile()) return null;
		try {
			return XMLUtilities.getDocument(new FileReader(file), createResolver());
		} catch (FileNotFoundException e) {
			return null;
		}
	}

	XMLEntityResolver createResolver() {
		XMLEntityResolver resolver = XMLEntityResolver.getInstance();
		return resolver;		
	}
	
	void loadGroup(MetaGroup g, IConfigurationElement element, ResourceBundle properties) {
		IConfigurationElement[] cs = element.getChildren();
		for (int i = 0; i < cs.length; ++i) {
			if (META_TEMPLATE.equals(cs[i].getName())) {
				MetaClassTemplate t = g.addMetaClassTemplate(cs[i].getAttribute(AXIS));
				loadClass(t, cs[i], properties);
			}
		}
	}

	void loadGroup(MetaGroup g, Element element, ResourceBundle properties) {
		Element[] cs = XMLUtilities.getChildren(element, META_TEMPLATE);
		for (int i = 0; i < cs.length; ++i) {
			String axis = cs[i].getAttribute(AXIS);
			if (axis != null && properties != null && axis.startsWith(PREFIX))  axis = properties.getString(axis);
			MetaClassTemplate t = g.addMetaClassTemplate(axis);
			loadClass(t, cs[i], properties);
		}
	}
	
	void loadClass(MetaClassTemplate t, IConfigurationElement element, ResourceBundle properties) {
		String displayName = element.getAttribute(DISPLAY_NAME);
		if (displayName != null && properties != null && displayName.startsWith(PREFIX)) displayName = properties.getString(displayName);
		t.setDisplayName(displayName); 
		String xEntity = element.getAttribute(ENTITY);
		if (xEntity != null && properties != null && xEntity.startsWith(PREFIX)) xEntity = properties.getString(xEntity);
		t.setXEntity(xEntity);
		IConfigurationElement[] childs = element.getChildren();
		ArrayList<String> is = new ArrayList<String>();
		for (int i = 0; i < childs.length; ++i) {
			IConfigurationElement child = childs[i];
			String name = child.getAttribute(NAME);
			if (name != null && properties != null && name.startsWith(PREFIX)) {
				name = properties.getString(name);
			}
			if (SUPER_CLASS.equals(child.getName())) {
				t.getSuperClass().setValue(name);
			} else if (INTERFACE.equals(child.getName())) {
				if(name != null && name.length() > 0) is.add(name); 
			}
		}
		t.getInterfaces().setValues(is.toArray(new String[0]));
	}

	void loadClass(MetaClassTemplate t, Element element, ResourceBundle properties) {
		String displayName = element.getAttribute(DISPLAY_NAME);
		if (displayName != null && displayName.startsWith(PREFIX)) {
			displayName = properties.getString(displayName);
		}
		t.setDisplayName(displayName); 
		String xEntity = element.getAttribute(ENTITY);
		if (xEntity != null && xEntity.startsWith(PREFIX)) {
			xEntity = properties.getString(xEntity);
		}
		t.setXEntity(xEntity);
		NodeList cs = element.getChildNodes();
		ArrayList<String> is = new ArrayList<String>();
		for (int i = 0; i < cs.getLength(); ++i) {
			Node cn = cs.item(i);
			if(cn.getNodeType() != Node.ELEMENT_NODE) continue;
			Element child = (Element)cn;
			String name = child.getAttribute(NAME);
			if (name != null &&  name.startsWith(PREFIX)) {
				name = properties.getString(name);
			}
			if (SUPER_CLASS.equals(child.getNodeName())) {
				t.getSuperClass().setValue(name);
			} else if (INTERFACE.equals(child.getNodeName())) {
				if(name != null && name.length() > 0) is.add(name); 
			}
		}
		t.getInterfaces().setValues(is.toArray(new String[is.size()]));
	}

	void saveConfiguration(MetaConfiguration c, Element element) {
		Iterator<MetaGroup> gs = c.getMetaTemplateGroups().iterator();
		while(gs.hasNext()) {
			MetaGroup g = (MetaGroup)gs.next();
			if(g.isOverriding()) {
				Element ge = XMLUtilities.createElement(element, META_TEMPLATE_GROUP);
				ge.setAttribute(URI, g.getUri());
				Iterator<MetaClassTemplate> ts = g.getTemplates().iterator();
				while(ts.hasNext()) {
					MetaClassTemplate t = (MetaClassTemplate)ts.next();
					if(t.isOverriding()) {
						Element te = XMLUtilities.createElement(ge, META_TEMPLATE);
						if(t.getAxis() != null) {
							te.setAttribute(AXIS, t.getAxis());
						}
						if(t.getDisplayName() != null) {
							te.setAttribute(DISPLAY_NAME, t.getDisplayName());
						}
						if(t.getXEntity() != null) {
							te.setAttribute(ENTITY, t.getXEntity());
						}
						MetaValue superClass = t.getSuperClass();
						Element sce = XMLUtilities.createElement(te, SUPER_CLASS);
						sce.setAttribute(NAME, superClass.getValue());
						MetaValueList interfaces = t.getInterfaces();
						String[] is = interfaces.getValues();
						for (int i = 0; i < is.length; i++) {
							Element ise = XMLUtilities.createElement(te, INTERFACE);
							ise.setAttribute(NAME, is[i]);
						}
					}
				}
			}
		}
	}

}
