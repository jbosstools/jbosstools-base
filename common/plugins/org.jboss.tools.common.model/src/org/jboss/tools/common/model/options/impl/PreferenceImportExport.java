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
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.jboss.tools.common.model.options.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.util.FileUtil;

public class PreferenceImportExport implements Preferences.IPropertyChangeListener {
	static String MODEL_PREFERENCES = "model_preferences";
	static PreferenceImportExport instance;
	
	public static PreferenceImportExport getInstance() {
		if(instance == null) {
			instance = new PreferenceImportExport();
		}
		return instance;
	}
	
	int lock = 0;
	
	public PreferenceImportExport() {
		Runnable r = new Runnable() {
			public void run() {
				getPreferences().addPropertyChangeListener(PreferenceImportExport.this);
			}
		};
		new Thread(r).start();
	}
	
	public void lock() {
		lock++;
	}
	
	public void unlock() {
		if(lock > 0) lock--;
	}
	
	public boolean isLocked() {
		return lock > 0;
	}

	public void propertyChange(PropertyChangeEvent event) {
		if(isLocked()) return;
		if(!event.getProperty().equals(MODEL_PREFERENCES)) return;
		String newValue = (String)event.getNewValue();
		XStudioLoaderPeer peer = XStudioLoaderPeer.instance();
		File f = peer.getProjectPreferencesFile();
		if(f != null) {
			String text = f.exists() ? FileUtil.readFile(f) : null;
			if(newValue != null && newValue.equals(text)) return;
			if(newValue == null && text == null) return;
			if(newValue != null) {
				FileUtil.writeFile(f, newValue);
			} else {
				f.delete();
			}
			lock();
			try {
				PreferenceModelUtilities.getPreferenceModel().load();
			} finally {
				unlock();
			}			
		}
	}
	
	private Preferences getPreferences() {
		return ModelPlugin.getDefault().getPluginPreferences();
	}
	
	public void apply(File f) {
		if(isLocked()) return;
		lock();
		try {
			if(f == null || !f.exists()) return;
			String text = FileUtil.readFile(f);
			if(text != null && text.length() > 0) {
				String oldValue = getPreferences().getString(MODEL_PREFERENCES);
				if(text.equals(oldValue)) return;
				getPreferences().setValue(MODEL_PREFERENCES, text);
			}
		} finally {
			unlock();
		}
	}
	
}
