package org.jboss.tools.runtime.core.model;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.util.RuntimePathPreferenceIO;

public class RuntimeModel {
	public static final String RUNTIME_PATHS = "runtimePaths";

	private ListenerList runtimePathChangeChangeListeners;
	private Set<RuntimePath> runtimePaths;
	private IEclipsePreferences preferences;
	
	public RuntimeModel() {
		runtimePaths = new HashSet<RuntimePath>();
	}
	
	public RuntimeModel(IEclipsePreferences preferences) {
		this.preferences = preferences;
		reloadRuntimePathsFromPreferences();
	}

	public void reloadRuntimePathsFromPreferences() {
		String runtimes = preferences.get(RUNTIME_PATHS, null);
		if (runtimes != null && !runtimes.isEmpty()) {
			runtimePaths = RuntimePathPreferenceIO.loadRuntimePathsFromPreferenceString(runtimes);		
		} else {
			runtimePaths = new HashSet<RuntimePath>();
		}
		fireRuntimePathsChanged();
	}
	
	public void saveRuntimePaths() {
		if (runtimePaths == null || preferences == null)
			return;
		
		try {
			String runtimes = RuntimePathPreferenceIO.getPreferenceOutputString(runtimePaths);
			preferences.put(RUNTIME_PATHS, runtimes);
			preferences.flush();
			fireRuntimePathsChanged();
		} catch (Exception e) {
			RuntimeCoreActivator.getDefault().logError(e);
		}
	}
	
	public synchronized void setRuntimePaths(RuntimePath[] set) {
		if( set == null ) {
			reloadRuntimePathsFromPreferences();
		} else {
			HashSet<RuntimePath> s = new HashSet<RuntimePath>();
			s.addAll(Arrays.asList(set));
			runtimePaths = s;
			saveRuntimePaths();
		}
	}
	
	public void addRuntimePath(RuntimePath path) {
		runtimePaths.add(path);
		saveRuntimePaths();
	}
	public void removeRuntimePath(RuntimePath path) {
		runtimePaths.remove(path);
		saveRuntimePaths();
	}
	
	public synchronized RuntimePath[] getRuntimePaths() {
		return (RuntimePath[]) runtimePaths.toArray(new RuntimePath[runtimePaths.size()]);
	}
	
	public synchronized void addRuntimePathChangeListener(IRuntimePathChangeListener listener) {
		if (runtimePathChangeChangeListeners == null)
			runtimePathChangeChangeListeners = new ListenerList();
		runtimePathChangeChangeListeners.add(listener);
	}
	
	public synchronized void removeRuntimePathChangeListener(IRuntimePathChangeListener listener) {
		if (runtimePathChangeChangeListeners == null)
			return;
		runtimePathChangeChangeListeners.remove(listener);
	}
	
	private void fireRuntimePathsChanged() {
		if (runtimePathChangeChangeListeners != null) {
			Object[] listeners = runtimePathChangeChangeListeners.getListeners();
			for (Object listener:listeners ) {
				IRuntimePathChangeListener runtimePathChangeChangeListener = (IRuntimePathChangeListener) listener;
				runtimePathChangeChangeListener.changed();
			}
		}
	}

}
