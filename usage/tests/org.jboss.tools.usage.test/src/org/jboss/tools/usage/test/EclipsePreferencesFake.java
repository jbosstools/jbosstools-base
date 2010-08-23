/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferenceNodeVisitor;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class EclipsePreferencesFake implements IEclipsePreferences {

	private Map<String, String> preferences;

	public EclipsePreferencesFake() {
		this.preferences = new HashMap<String, String>();
	}

	public void accept(IPreferenceNodeVisitor visitor) throws BackingStoreException {
		throw new UnsupportedOperationException();
	}

	public void addNodeChangeListener(INodeChangeListener listener) {
		throw new UnsupportedOperationException();
	}

	public void addPreferenceChangeListener(IPreferenceChangeListener listener) {
		throw new UnsupportedOperationException();
	}

	public Preferences node(String path) {
		throw new UnsupportedOperationException();
	}

	public void removeNode() throws BackingStoreException {
		throw new UnsupportedOperationException();
	}

	public void removeNodeChangeListener(INodeChangeListener listener) {
		throw new UnsupportedOperationException();
	}

	public void removePreferenceChangeListener(IPreferenceChangeListener listener) {
		throw new UnsupportedOperationException();
	}

	public String absolutePath() {
		throw new UnsupportedOperationException();
	}

	public String[] childrenNames() throws BackingStoreException {
		throw new UnsupportedOperationException();
	}

	public void clear() throws BackingStoreException {
		this.preferences.clear();
	}

	public void flush() throws BackingStoreException {
	}

	public String get(String key, String defaultValue) {
		String value = preferences.get(key);
		if (value == null) {
			value = defaultValue;
		} 
		return value;
	}

	public boolean getBoolean(String key, boolean defaultValue) {
		throw new UnsupportedOperationException();
	}

	public byte[] getByteArray(String key, byte[] defaultValue) {
		throw new UnsupportedOperationException();
	}

	public double getDouble(String key, double defaultValue) {
		throw new UnsupportedOperationException();
	}

	public float getFloat(String key, float defaultValue) {
		throw new UnsupportedOperationException();
	}

	public int getInt(String key, int defaultValue) {
		throw new UnsupportedOperationException();
	}

	public long getLong(String key, long defaultValue) {
		String value = preferences.get(key);
		if (value != null) {
			return Long.valueOf(value);
		} else {
			return defaultValue;
		}
	}

	public String[] keys() throws BackingStoreException {
		throw new UnsupportedOperationException();
	}

	public String name() {
		throw new UnsupportedOperationException();
	}

	public boolean nodeExists(String pathName) throws BackingStoreException {
		throw new UnsupportedOperationException();
	}

	public Preferences parent() {
		throw new UnsupportedOperationException();
	}

	public void put(String key, String value) {
		preferences.put(key, value);
	}

	public void putBoolean(String key, boolean value) {
		throw new UnsupportedOperationException();
	}

	public void putByteArray(String key, byte[] value) {
		throw new UnsupportedOperationException();
	}

	public void putDouble(String key, double value) {
		throw new UnsupportedOperationException();
	}

	public void putFloat(String key, float value) {
		throw new UnsupportedOperationException();
	}

	public void putInt(String key, int value) {
		throw new UnsupportedOperationException();
	}

	public void putLong(String key, long value) {
		preferences.put(key, String.valueOf(value));
	}

	public void remove(String key) {
		throw new UnsupportedOperationException();
	}

	public void sync() throws BackingStoreException {
		// ignore
	}
}