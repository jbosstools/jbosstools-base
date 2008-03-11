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
package org.jboss.tools.common.model.options.impl;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import org.w3c.dom.*;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.options.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.osgi.framework.Bundle;

public class XStudioDataLoaderImpl implements SharableConstants {
    private SharableElement studio = null;

    private XStudioLoaderPeer peer = XStudioLoaderPeer.instance();

    public XStudioDataLoaderImpl() {}

    public void load(XModelObject _studio) {
        peer.init(_studio);
        studio = (SharableElement)_studio;
        peer.setIsLoadingOn(true);
        studio.setScope(PROJECT);
        new SharableLoaderImpl().loadSystemSharable(studio);
        load(LIST[0], readGeneral());
        boolean e = false;
        for (int i = 1; i < LIST.length; i++) {
        	File[] fs = peer.getFilesForScope(LIST[i]);
            for (int j = 0; j < fs.length; j++) e = load(LIST[i], fs[j]);
        }
        mergeGeneralToProject((SharableElementImpl)studio, !e);
        peer.setIsLoadingOn(false);
        _studio.setModified(false);
        
        File f = peer.getProjectPreferencesFile();
        if(f != null && f.exists()) {
    		PreferenceImportExport.getInstance().apply(f);
        }
    }

    private boolean load(String scopename, File file) {
        Element element = XMLUtil.getElement(file.getAbsolutePath());
        return load(scopename, element);
    }

    private boolean load(String scopename, InputStream s) {
        Element element = XMLUtil.getElement(s);
        return load(scopename, element);
    }

    private boolean load(String scopename, Element element) {
        if(element == null) return false;
        SharableLoaderImpl loader = new SharableLoaderImpl();
        loader.loadSharable(element, studio, scopename);
        return true;
    }

    public boolean save(XModelObject _studio) {
        if(!_studio.isModified()) return true;
        peer.init(_studio);
        studio = (SharableElement)_studio;
        for (int i = 0; i < LIST.length; i++) {
            if(i == 1) saveProject();
        }
        _studio.setModified(false);
        return true;
    }
    
    private void saveProject() {
		File[] fs = peer.getFilesForScope(PROJECT);
		boolean isPaletteModified = studio.getChildByPath("Palette").isModified() || studio.getChildByPath("Icons").isModified();
		boolean isPreferencesModified = studio.getChildByPath("Options").isModified();
    	if(!fs[0].exists() || isPaletteModified) savePalette(fs[0]);
    	if(!fs[1].exists() || isPreferencesModified) savePreferences(fs[1]);
    }
    
    private void savePalette(File f) {
		if(handleReadOnly(studio, PROJECT, f) != 0) return;
    	save(studio, PROJECT, f, new String[]{"Palette", "XStudioIcons"});
		studio.getChildByPath("Palette").setModified(false);
		studio.getChildByPath("Icons").setModified(false);
	}

	private void savePreferences(File f) {
		if(handleReadOnly(studio, LIST[1], f) != 0) return;
		save(studio, PROJECT, f, new String[]{"Options"});
		studio.getChildByPath("Options").setModified(false);
	}

    private void save(SharableElement q, String scopename, File f, String[] names) {
        if(f.exists() && (!f.isFile() || !f.canWrite())) return;
        try {
            if(!f.exists()) f.createNewFile();
        } catch (Exception e1) {
        	ModelPlugin.getPluginLog().logError("XStudioDataLoaderImpl:save:Cannot create file:" + e1.getMessage());
            return;
        }
        Element e = XMLUtil.createDocumentElement("dummyroot");
        new SharableLoaderImpl().saveSharable(e, q, scopename);
        NodeList x = e.getElementsByTagName(XSTUDIO);
        if(x == null || x.getLength() == 0) return;
		Element xs = (Element)x.item(0);
		check(xs.getChildNodes(), names);
        try {
            XModelObjectLoaderUtil.serialize(xs, f.getAbsolutePath());
        } catch (Exception e2) {
        	ModelPlugin.getPluginLog().logError(e2);
        }
    }
    
    private void check(NodeList l, String[] names) {
    	Node[] ns = new Node[l.getLength()];
    	for (int i = 0; i < ns.length; i++) ns[i] = l.item(i);
		for (int i = 0; i < ns.length; i++) check(ns[i], names);    	
    }    
    private void check(Node n, String[] names) {
    	if(n.getNodeType() != Node.ELEMENT_NODE) return;
    	for (int i = 0; i < names.length; i++)
    	  if(names[i].equals(n.getNodeName())) return;
    	 n.getParentNode().removeChild(n);
    }

    private void mergeGeneralToProject(SharableElementImpl object, boolean merge_all) {
        object.merge(GENERAL, PROJECT, merge_all);
    }

    private int handleReadOnly(XModelObject o, String scope, File f) {
        if(!PROJECT.equals(scope)) return 1;
        int i = 0;
        while(i == 0 && f.exists() && !f.canWrite())
          i = o.getModel().getService().showDialog("Question",
                getReadOnlyMessage(f), new String[]{"Retry", "Discard"}, null,
                org.jboss.tools.common.model.ServiceDialog.QUESTION);
        return i;
    }

    private String getReadOnlyMessage(File f) {
        return "File " + f.getAbsolutePath() + " is read-only.\n" +
               "Please make it writable to allow for saving options.";
    }
    
    private InputStream readGeneral() {
    	try {
            Bundle b = Platform.getBundle(ModelPlugin.PLUGIN_ID);
        	URL u = b.getResource("meta/options_general.xml");
            URLConnection c = u.openConnection();
            return c.getInputStream();
    	} catch (Exception e) {
    		ModelPlugin.getPluginLog().logError(e);
    		return null;
    	}
    }

}
