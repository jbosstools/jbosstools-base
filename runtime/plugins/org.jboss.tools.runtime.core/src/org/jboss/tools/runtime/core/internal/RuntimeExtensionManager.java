/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimesProvider;
import org.jboss.tools.runtime.core.model.IRuntimeDetectionResolution;
import org.jboss.tools.runtime.core.model.IRuntimeDetectionResolutionProvider;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.IRuntimeDetectorDelegate;
import org.jboss.tools.runtime.core.model.IRuntimeInstaller;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimeDetectionProblem;

public class RuntimeExtensionManager {
	// Extension points 
	private static final String RUNTIME_DETECTOR_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeDetectors"; //$NON-NLS-1$
	public static final String DOWNLOAD_RUNTIMES_PROVIDER_EXTENSION_ID = "org.jboss.tools.runtime.core.downloadRuntimeProvider"; //$NON-NLS-1$
	private static final String RUNTIME_INSTALLER_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeInstaller"; //$NON-NLS-1$
	private static final String RUNTIME_PROBLEM_RESOLUTION_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeDetectionResolutionProvider"; //$NON-NLS-1$
	// Member variables
	private IDownloadRuntimesProvider[] downloadRuntimeProviders = null;
	private Set<IRuntimeDetector> runtimeDetectors;
	

	// property keyys for download runtime provider ext pt. 
	private static final String NAME = "name"; //$NON-NLS-1$
	private static final String CLAZZ = "class"; //$NON-NLS-1$
	// property keys for runtime detector ext.pt.
	private static final String PREFERENCE_ID = "preferenceId"; //$NON-NLS-1$
	private static final String ID = "id"; //$NON-NLS-1$
	private static final String ENABLED = "enabled"; //$NON-NLS-1$
	private static final String PRIORITY = "priority"; //$NON-NLS-1$

	private static RuntimeExtensionManager manager = null;
	public static RuntimeExtensionManager getDefault() {
		if( manager == null )
			manager = new RuntimeExtensionManager();
		return manager;
	}
	
	/**
	 * This method will load runtime detectors from the extension
	 * point, AND set its enablement based on values from the 
	 * preferences.
	 * 
	 * Protected only for testing purposes
	 * 
	 * @return
	 */
	protected Set<IRuntimeDetector> loadInitializedRuntimeDetectors() {
		Set<IRuntimeDetector> tmp = loadDeclaredRuntimeDetectors();
		initializeRuntimeDetectorEnablement(tmp);
		return tmp;
	}
	
	public synchronized Set<IRuntimeDetector> getRuntimeDetectors() {
		if (runtimeDetectors == null) {
			runtimeDetectors = RuntimeExtensionManager.getDefault().loadInitializedRuntimeDetectors();
		}
		return runtimeDetectors;
	}


	public IRuntimeDetector findRuntimeDetector(String id) {
		for (IRuntimeDetector detector:getRuntimeDetectors()) {
			if (id.equals(detector.getId())) {
				return detector;
			}
		}
		return null;
	}
	
	private void initializeRuntimeDetectorEnablement(Set<IRuntimeDetector> set) {
		String[] enabledDetectors = RuntimeCorePreferences.getDefault().getEnabledRuntimeDetectors();
		String[] disabledDetectors = RuntimeCorePreferences.getDefault().getDisabledRuntimeDetectors();
		
		boolean allEnabled = false;
		if (enabledDetectors == null) {
			allEnabled = true;
		}
		
		enabledDetectors = (enabledDetectors == null ? new String[0] : enabledDetectors);
		disabledDetectors = (disabledDetectors == null ? new String[0] : disabledDetectors);
		List<String> enabled = Arrays.asList(enabledDetectors);
		List<String> disabled = Arrays.asList(disabledDetectors);
		for (IRuntimeDetector detector : set) {
			if( allEnabled || enabled.contains(detector.getId())) {
				((RuntimeDetector)detector).setEnabled(true);
			} else if( disabled.contains(detector.getId())) {
				((RuntimeDetector)detector).setEnabled(false);
			} // Else use the default for the (assumed new) detector
		}
	}
	
	/**
	 *  This method will do a full load and actually instantiate the classes
	 *  It will *NOT* set the enablement for the runtime detectors
	 *  
	 *  This method should not be public :( 
	 * @return
	 */
	public Set<IRuntimeDetector> loadDeclaredRuntimeDetectors() {
		Set<IRuntimeDetector> declared = new TreeSet<IRuntimeDetector>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry
				.getExtensionPoint(RUNTIME_DETECTOR_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IRuntimeDetector dec = loadOneDeclaredRuntimeDetector(configurationElements[j]); 
				if( !declared.contains(dec)) {
					declared.add(dec);
				}
			}
		}
		return declared;
	}
	
	// This method will load one detector from a configuration element
	private IRuntimeDetector loadOneDeclaredRuntimeDetector(IConfigurationElement configurationElement) {
		String name = configurationElement.getAttribute(NAME);
		String preferenceId = configurationElement.getAttribute(PREFERENCE_ID);
		String id = configurationElement.getAttribute(ID);
		String priorityString = configurationElement
				.getAttribute(PRIORITY);
		String enabled = configurationElement.getAttribute(ENABLED);
		
		int priority;
		try {
			priority = Integer.parseInt(priorityString);
		} catch (Exception ex) {
			priority = Integer.MAX_VALUE;
		}

		IRuntimeDetectorDelegate delegate = null;
		try {
			delegate = (IRuntimeDetectorDelegate) configurationElement.createExecutableExtension(CLAZZ);
			RuntimeDetector detector = new RuntimeDetector(
					name, id, preferenceId, priority, delegate);
			detector.setEnabled(Boolean.parseBoolean(enabled));
			return detector;
		} catch (CoreException e) {
			RuntimeCoreActivator.pluginLog().logError(e);
			return new InvalidRuntimeDetector(name, id, preferenceId, priority);
		}
	}
	
	/**
	 * This method does not benefit from progress monitors. 
	 * Use at your own risk. 
	 * @return
	 */
	public Map<String, DownloadRuntime> getDownloadRuntimes() {
		return getDownloadRuntimes( new NullProgressMonitor() );
	}
	
	private Map<String, DownloadRuntime> cachedDownloadRuntimes = null;
	public Map<String, DownloadRuntime> getDownloadRuntimes(IProgressMonitor monitor) {

		// Cache for now, since we still fetch remote files
		// Once fetching remote files is removed, we no longer
		// need to cache this, and in fact should not. 
		// Individual providers can cache on their own, or not, a they wish
		// We still return the actual data map. This is pretty bad. 
		if( cachedDownloadRuntimes == null ) {
			Map<String, DownloadRuntime> tmp = loadDownloadRuntimes(monitor);
			if( monitor.isCanceled()) {
				// Do not cache, as the list is incomplete and should be loaded again.
				return tmp;
			}
			cachedDownloadRuntimes = tmp;
		}
		return cachedDownloadRuntimes;
	}

	/**
	 * This method may be long-running. This signature is not reccommended for use. 
	 * Please use hte signature with a progress monitor instead
	 * @deprecated
	 * @param id
	 * @return
	 */
	public DownloadRuntime findDownloadRuntime(String id) {
		Map<String, DownloadRuntime> runtimes = getDownloadRuntimes();
		return findDownloadRuntime(id, new NullProgressMonitor());
	}
	
	public DownloadRuntime findDownloadRuntime(String id, IProgressMonitor monitor) {
		Map<String, DownloadRuntime> runtimes = getDownloadRuntimes(monitor);
		return findDownloadRuntime(id, runtimes);
	}
	
	private DownloadRuntime findDownloadRuntime(String id, Map<String, DownloadRuntime> runtimes) {
		if( id == null )
			return null;
		
		DownloadRuntime rt = runtimes.get(id);
		if( rt != null )
			return rt;
		Collection<DownloadRuntime> rts = runtimes.values();
		Iterator<DownloadRuntime> i = rts.iterator();
		while(i.hasNext()) {
			DownloadRuntime i1 = i.next();
			Object propVal = i1.getProperty(DownloadRuntime.PROPERTY_ALTERNATE_ID);
			if( propVal != null ) {
				if( propVal instanceof String[]) {
					String[] propVal2 = (String[]) propVal;
					for( int it = 0; it < propVal2.length; it++ ) {
						if( id.equals(propVal2[it]))
							return i1;
					}
				} else if( propVal instanceof String ) {
					if( id.equals(propVal))
						return i1;
				}
			}
		}
		return null;
	}
	
	private Map<String, DownloadRuntime> loadDownloadRuntimes(IProgressMonitor monitor) {
		HashMap<String, DownloadRuntime> tmp = new HashMap<String, DownloadRuntime>();
		monitor.beginTask("Loading Downloadable Runtimes", 300);
		loadDownloadableRuntimesFromProviders(tmp, new SubProgressMonitor(monitor, 300));
		return tmp;
	}	
	

	/**
	 * This method is NOT PUBLIC. 
	 * It is only exposed for TESTING purposes.
	 * 
	 * @param map
	 */
	public void loadDownloadableRuntimesFromProviders(Map<String, DownloadRuntime> map, IProgressMonitor monitor) {
		IDownloadRuntimesProvider[] providers = getDownloadRuntimeProviders();
		monitor.beginTask("Loading Download Runtime Providers", providers.length * 100);
		for( int i = 0; i < providers.length && !monitor.isCanceled(); i++ ) {
			IProgressMonitor inner = new SubProgressMonitor(monitor, 100);
			DownloadRuntime[] runtimes = providers[i].getDownloadableRuntimes(null, inner);
			if( runtimes != null ) {
				for( int j = 0; j < runtimes.length; j++ ) {
					if( runtimes[j] != null )
						map.put(runtimes[j].getId(), runtimes[j]);
				}
			}
			inner.done();
		}
	}
	
	private IDownloadRuntimesProvider[] getDownloadRuntimeProviders() {
		if( downloadRuntimeProviders == null )
			downloadRuntimeProviders = loadDownloadRuntimeProviders();
		return downloadRuntimeProviders;
	}
	
	private IDownloadRuntimesProvider[] loadDownloadRuntimeProviders() {
		ArrayList<IDownloadRuntimesProvider> list = new ArrayList<IDownloadRuntimesProvider>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry
				.getExtensionPoint(DOWNLOAD_RUNTIMES_PROVIDER_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement configurationElement = configurationElements[j];
				try {
					IDownloadRuntimesProvider provider = (IDownloadRuntimesProvider)configurationElement.createExecutableExtension(CLAZZ);
					list.add(provider);
				} catch(CoreException ce) {
					RuntimeCoreActivator.pluginLog().logError("Error loading download runtime provider", ce);
				}
			}
		}
		return (IDownloadRuntimesProvider[]) list.toArray(new IDownloadRuntimesProvider[list.size()]);
	}	

	
	private static class RuntimeInstallerWrapper {
		private String id;
		private IRuntimeInstaller installer;
		public RuntimeInstallerWrapper(String id, IRuntimeInstaller installer) {
			this.id = id;
			this.installer = installer;
		}
		public String getId() {
			return id;
		}
		public IRuntimeInstaller getInstaller() {
			return installer;
		}
	}
	private ArrayList<RuntimeInstallerWrapper> installers;
	private ArrayList<RuntimeInstallerWrapper> loadInstallers() {
		ArrayList<RuntimeInstallerWrapper> list = new ArrayList<RuntimeInstallerWrapper>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry.getExtensionPoint(RUNTIME_INSTALLER_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement configurationElement = configurationElements[j];
				try {
					IRuntimeInstaller installer = (IRuntimeInstaller)configurationElement.createExecutableExtension(CLAZZ);
					String id = configurationElement.getAttribute("id");
					list.add(new RuntimeInstallerWrapper(id, installer));
				} catch(CoreException ce) {
					RuntimeCoreActivator.pluginLog().logError("Error loading runtime installer", ce);
				}
			}
		}
		return list;
	}
	
	/**
	 * Get a runtime installer by the given id. 
	 * 
	 * @param id
	 * @return
	 */
	public IRuntimeInstaller getRuntimeInstaller(String id) {
		if( installers == null ) {
			installers = loadInstallers();
		}
		Iterator<RuntimeInstallerWrapper> it = installers.iterator();
		while(it.hasNext()) {
			RuntimeInstallerWrapper w = it.next();
			if(id.equals(w.getId())) {
				return w.getInstaller();
			}
		}
		return null;
	}
	
	
	private ArrayList<ProblemResolutionProviderWrapper> problemResolutionProviders;
	private ArrayList<ProblemResolutionProviderWrapper> loadProblemResolvers() {
		ArrayList<ProblemResolutionProviderWrapper> list = new ArrayList<ProblemResolutionProviderWrapper>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry.getExtensionPoint(RUNTIME_PROBLEM_RESOLUTION_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement configurationElement = configurationElements[j];
				try {
					IRuntimeDetectionResolutionProvider provider = (IRuntimeDetectionResolutionProvider)configurationElement.createExecutableExtension(CLAZZ);
					String filter = configurationElement.getAttribute("runtimeDetectorId");
					String weight = configurationElement.getAttribute("weight");
					int weightInt = 10;
					if( weight != null ) {
						try {
							weightInt = Integer.parseInt(weight);
						} catch(NumberFormatException nfe) {
							// ignore
						}
					}
					list.add(new ProblemResolutionProviderWrapper(weightInt, filter, provider));
				} catch(CoreException ce) {
					RuntimeCoreActivator.pluginLog().logError("Error loading runtime problem resolution provider", ce);
				}
			}
		}
		
		list.sort(new Comparator<ProblemResolutionProviderWrapper>() {
			public int compare(ProblemResolutionProviderWrapper arg0, ProblemResolutionProviderWrapper arg1) {
				return arg0.getWeight() - arg1.getWeight();
			}});
		return list;
	}
	
	
	public IRuntimeDetectionResolution[] findResolutions(RuntimeDetectionProblem problem, RuntimeDefinition def) {
		if( problemResolutionProviders == null ) {
			problemResolutionProviders = loadProblemResolvers();
		}
		IRuntimeDetector detector = def.getDetector();
		String detectorId = detector == null ? null : detector.getId();
		
		Iterator<ProblemResolutionProviderWrapper> it = problemResolutionProviders.iterator();
		ArrayList<IRuntimeDetectionResolution> ret = new ArrayList<IRuntimeDetectionResolution>();
		while(it.hasNext()) {
			ProblemResolutionProviderWrapper wrap = it.next();
			if( detectorId == null || wrap.getDetectorFilter() == null || wrap.getDetectorFilter().equals(detectorId)) {
				IRuntimeDetectionResolution[] resolutions = wrap.getProvider().getResolutions(problem, def);
				if( resolutions != null ) {
					ret.addAll(Arrays.asList(resolutions));
				}
			}
		}
		return (IRuntimeDetectionResolution[]) ret.toArray(new IRuntimeDetectionResolution[ret.size()]);
	}
	
}
