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
package org.jboss.tools.common.propertieseditor.bundlemodel;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class BundleModel {
	IFile main;
	XModelObject object = PreferenceModelUtilities.getPreferenceModel().createModelObject("FilePROPERTIES", null); //$NON-NLS-1$
	String currentLocale = ""; //$NON-NLS-1$
	Set<String> locales = new TreeSet<String>();
	Set<String> removedLocales = new HashSet<String>();
	boolean isModified;
	
	public BundleModel() {}
	
	public void dispose() {
		modificationListener = null;
		if (locales!=null) locales.clear();
		locales = null;
		if (removedLocales!=null) removedLocales.clear();
		removedLocales = null;
		main = null;
	}
	
	public void setMainFile(IFile f) {
		main = f;
	}
	
	public String getName() {
		String name = main.getName();
		if(name.endsWith(".properties")) name = name.substring(0, name.length() - ".properties".length()); //$NON-NLS-1$ //$NON-NLS-2$
		return name;
	}
	
	public IFile getMainFile() {
		return main;
	}
	
	public XModelObject getModelObject() {
		return object;
	}
	
	public IFile getFile(String locale) {
		if(locale.length() == 0) return main;
		String name = getName() + "_" + locale + ".properties"; //$NON-NLS-1$ //$NON-NLS-2$
		return main.getParent().getFile(new Path("/" + name));  //$NON-NLS-1$
	}
	
	public String[] getLocales() {
		return (String[])locales.toArray(new String[0]);
	}
	
	public void setCurrentLocale(String locale) {
		currentLocale = locale;
		PropertyModel[] ps = getPropertyModelArray();
		for (int i = 0; i < ps.length; i++) ps[i].setLocale(locale);
	}
	
	public PropertyModel[] getPropertyModelArray() {
		XModelObject[] cs = object.getChildren();
		PropertyModel[] ps = new PropertyModel[cs.length];
		for (int i = 0; i < ps.length; i++) 
			ps[i] = getPropertyModel(cs[i], currentLocale);
		return ps;
	}
	
	public PropertyModel createPropertyModel(String name, String value, String locale) {
		XModelObject o = PreferenceModelUtilities.getPreferenceModel().createModelObject("Property", null); //$NON-NLS-1$
		o.setAttributeValue("name", name); //$NON-NLS-1$
		o.setAttributeValue("value", value); //$NON-NLS-1$
		object.addChild(o);
		return getPropertyModel(o, locale);
	}
	
	public PropertyModel getPropertyModel(String name) {
		return getPropertyModel(object.getChildByPath(name), currentLocale);
	}
	
	public PropertyModel getPropertyModel(XModelObject o, String locale) {
		if(o == null) return null;
		PropertyModel pm = (PropertyModel)o.getObject("propertyModel"); //$NON-NLS-1$
		if(pm == null) {
			pm = new PropertyModel(this);
			pm.setModelObject(o);
			if(locale != null) pm.setValue(locale, o.getAttributeValue("value")); //$NON-NLS-1$
			pm.setLocale(currentLocale);
		}
		return pm;
	}
	
	public String getCurrentLocale() {
		return currentLocale;
	}
	
	public void load() {
		locales.clear();
		removedLocales.clear();
		locales.add(""); //$NON-NLS-1$
		String name = getName();
		String[][] p = loadFile(main);
		if(p == null) return;
		for (int i = 0; i < p.length; i++) {
			createPropertyModel(p[i][0], p[i][1], ""); //$NON-NLS-1$
		}
		IResource[] rs = new IResource[0];
		try { 
			rs = main.getParent().members();
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		for (int i = 0; i < rs.length; i++) {
			if(!(rs[i] instanceof IFile)) continue;
			IFile f = (IFile)rs[i];
			String nm = rs[i].getLocation().lastSegment().toString();
			if(!nm.startsWith(name) || !nm.endsWith(".properties") || nm.indexOf("_") < 0) continue; //$NON-NLS-1$ //$NON-NLS-2$
			nm = nm.substring(0, nm.length() - ".properties".length()); //$NON-NLS-1$
			String locale = nm.substring(nm.indexOf("_") + 1); //$NON-NLS-1$
			locales.add(locale);
			p = loadFile(f);
			if(p == null) continue;
			for (int j = 0; j < p.length; j++) {
				PropertyModel pm = getPropertyModel(p[j][0]);
				if(pm == null) {
					pm = createPropertyModel(p[j][0], p[j][1], locale); 
				} else {
					pm.setValue(locale, p[j][1]);
				}
			}
		}			
	}
	
	private String[][] loadFile(IFile file) {
		if(!file.exists()) return null;
		ArrayList<String> keys = new ArrayList<String>();
		ArrayList<String> values = new ArrayList<String>();
		Properties p = new Properties();
		InputStream s = null;
		try {
			s = file.getContents();
			BufferedReader in = new BufferedReader(new InputStreamReader(s, "8859_1")); //$NON-NLS-1$
			while (true) {
				String line = in.readLine();
				if(line == null) break;
				if(line.length() == 0) continue;
				char firstChar = line.charAt(0);
				if ((firstChar == '#') || (firstChar == '!')) continue;
				StringBuffer sb = new StringBuffer(line);
				while (continueLine(line)) {
					String nextLine = in.readLine();
					if(nextLine == null) nextLine = ""; //$NON-NLS-1$
					sb.append("\n"); //$NON-NLS-1$
					sb.append(line = nextLine);
				}
				ByteArrayInputStream is = new ByteArrayInputStream(sb.toString().getBytes());
				p.clear();
				try {
					p.load(is);
				} catch (IllegalArgumentException e1) {
					//ignore
				} catch (IOException e) {
					ModelUIPlugin.getPluginLog().logError(e);					
				}				
				Enumeration ee = p.keys();
				if(ee.hasMoreElements()) {
					String key = ee.nextElement().toString();
					keys.add(key);
					values.add(p.getProperty(key));
				}
			}
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} finally {
			if(s!=null) {
				try {
					s.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
		String[][] r = new String[keys.size()][2];
		for (int i = 0; i < r.length; i++) {
			r[i][0] = keys.get(i).toString();
			r[i][1] = values.get(i).toString();
		}
		return r;
	}
	private boolean continueLine(String line) {
		int slashCount = 0;
		int index = line.length() - 1;
		while((index >= 0) && (line.charAt(index--) == '\\'))
			slashCount++;
		return (slashCount % 2 == 1);
	}

	
	public void addLocale(String locale) {
		if(!locales.contains(locale)) {
			locales.add(locale);
			removedLocales.remove(locale);
			setModified(true);
		}
		setCurrentLocale(locale);
	}
	
	public void removeLocale(String locale) {
		if(locale.length() == 0 || !locales.contains(locale)) return;
		locales.remove(locale);
		removedLocales.add(locale);
		setModified(true);
	}
	
	public void save() {
		String[] ls = (String[])removedLocales.toArray(new String[0]);
		for (int i = 0; i < ls.length; i++) {
			IFile f = getFile(ls[i]);
			if(f.exists()) try {
				f.delete(true, true, null);
			} catch (CoreException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		removedLocales.clear();
		ls = (String[])locales.toArray(new String[0]);
		for (int i = 0; i < ls.length; i++) {
			IFile f = getFile(ls[i]);
			StringBuffer sb = new StringBuffer();
			PropertyModel[] ps = getPropertyModelArray();
//			String[][] r = new String[ps.length][2];
			for (int k = 0; k < ps.length; k++) {
				String key = ps[k].getName();
				String value = ps[k].getValue(ls[i]);
				if(ps[k].hasValue(ls[i])) {
					Properties p0 = new Properties();
					p0.setProperty(key, value);
					ByteArrayOutputStream os = new ByteArrayOutputStream();
					try {
						p0.store(os, null); 
					} catch (IOException e) {
						ModelUIPlugin.getPluginLog().logError(e);
						continue;
					}
					String s = os.toString();
					if(s.startsWith("#")) s = s.substring(s.indexOf('\n') + 1); //$NON-NLS-1$
					sb.append(s);
				}
			}		
			ByteArrayInputStream is = new ByteArrayInputStream(sb.toString().getBytes());
			try {
				 if(f.exists()) f.setContents(is, true, true, null);
				 else f.create(is, true, null);
			} catch (CoreException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		setModified(false);
	}
	
	public boolean isModified() {
		return isModified;
	}
	
	public interface ModificationListener {
		public void changed();
	}
	
	ModificationListener modificationListener;
	
	public void setModified(boolean b) {
		if(isModified == b) return;
		isModified = b;
		if(modificationListener != null) modificationListener.changed();
	}
	
	public void addModifiedListener(ModificationListener listener) {
		modificationListener = listener;
	}
	
	public void removeModifiedListener(ModificationListener listener) {
		modificationListener = null;
	}
	
	public boolean isEditable() {
		IFile f = getFile(currentLocale);
		if(f == null || !f.exists()) return true;
		return !f.isReadOnly();
	}
	
}
