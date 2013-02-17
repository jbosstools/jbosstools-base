package org.jboss.tools.runtime.core.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.util.FileUtils;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.IRuntimeDetectorDelegate;
import org.jboss.tools.runtime.core.util.ECFTransport;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class RuntimeExtensionManager {
	// Extension points 
	private static final String RUNTIME_DETECTOR_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeDetectors";
	public static final String DOWNLOAD_RUNTIMES_EXTENSION_ID = "org.jboss.tools.runtime.core.downloadruntimes"; //$NON-NLS-1$
	
	// JBoss Runtime files
	private static final String DOWNLOAD_RUNTIMES_FILE = "download_runtime.xml"; //$NON-NLS-1$
	
	// Extension point property keys
	
	private static final String URL = "url"; //$NON-NLS-1$
	private static final String DISCLAIMER = "disclaimer"; //$NON-NLS-1$
	private static final String LICENSE_URL = "licenseUrl";//$NON-NLS-1$
	private static final String SIZE = "size";//$NON-NLS-1$
	private static final String VERSION = "version";
	private static final String NAME = "name";
	
	private static final String PREFERENCE_ID = "preferenceId";
	private static final String ID = "id";
	private static final String ENABLED = "enabled";
	private static final String PRIORITY = "priority";

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
	 * @return
	 */
	public Set<IRuntimeDetector> loadInitializedRuntimeDetectors() {
		Set<IRuntimeDetector> tmp = loadDeclaredRuntimeDetectors();
		initializeRuntimeDetectorEnablement(tmp);
		return tmp;
	}
	
	private void initializeRuntimeDetectorEnablement(Set<IRuntimeDetector> set) {
		String[] enabledDetectors = RuntimeCorePreferences.getDefault().getEnabledRuntimeDetectors();
		boolean allEnabled = false;
		if (enabledDetectors == null) {
			allEnabled = true;
		}
		
		enabledDetectors = (enabledDetectors == null ? new String[0] : enabledDetectors);
		List<String> enabled = Arrays.asList(enabledDetectors);
		for (IRuntimeDetector detector : set) {
			boolean enableVal = allEnabled || enabled.contains(detector.getId());
			((RuntimeDetector)detector).setEnabled(enableVal);
		}
	}
	
	/**
	 *  This method will do a full load and actually instantiate the classes
	 *  It will *NOT* set the enablement for the runtime detectors
	 *  
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
			delegate = (IRuntimeDetectorDelegate) configurationElement.createExecutableExtension("class");
			RuntimeDetector detector = new RuntimeDetector(
					name, id, preferenceId, priority, delegate);
			detector.setEnabled(Boolean.parseBoolean(enabled));
			return detector;
		} catch (CoreException e) {
			RuntimeCoreActivator.getDefault().logError(e);
			return new InvalidRuntimeDetector(name, id, preferenceId, priority);
		}
	}
	
	public Map<String, DownloadRuntime> loadDownloadRuntimes() {
		HashMap<String, DownloadRuntime> tmp = new HashMap<String, DownloadRuntime>();
		loadExtensionDownloadableRuntimes(tmp);
		loadExternalDownloadableRuntimes(tmp);
		return tmp;
	}	
	
	private File getCacheFile() {
		IPath location = RuntimeCoreActivator.getDefault().getStateLocation();
		File cacheFile = new File(location.toFile(), DOWNLOAD_RUNTIMES_FILE);
		return cacheFile;
	}
	
	private long getCacheModified() {
		long cacheModified = 0;
		// This won't be a regression. First time it will simply fetch from remote
		File f = getCacheFile();
		if (f.isFile()) {
			cacheModified = f.lastModified();
		}
		return cacheModified;
		
	}
	private URL getUrl(String s) {
		try {
			URL url = new URL(s);
			return url;
		} catch(MalformedURLException murle) {
			return null;
		}
	}
	private long getRemoteModified(String urlString) {
		long urlModified = -1;
		try {
			URL url = getUrl(urlString);
			if( url != null )
				urlModified = ECFTransport.getInstance()
						.getLastModified(url);
		} catch (Exception e) {
			RuntimeCoreActivator.getDefault().logError(e);
			urlModified = -1;
		}
		return urlModified;
	}
	
	private void downloadRemoteRuntimeFile(String urlString) throws Exception { 
		URL url = getUrl(urlString);
		File tempFile = File.createTempFile(
				"download_runtimes", ".xml");  //$NON-NLS-1$//$NON-NLS-2$
		tempFile.deleteOnExit();
		OutputStream destination = new FileOutputStream(tempFile);
		IStatus status = ECFTransport.getInstance().download(
				DOWNLOAD_RUNTIMES_FILE, urlString, destination,
				new NullProgressMonitor());
		if (status.isOK() && url != null) {
			long cacheModified = ECFTransport.getInstance()
					.getLastModified(url);
			FileUtils.copyFile(tempFile, getCacheFile());
			tempFile.delete();
			getCacheFile().setLastModified(cacheModified);
		} else {
			RuntimeCoreActivator.getDefault().getLog().log(status);
		}
	}
	
	private void loadExtensionDownloadableRuntimes(HashMap<String, DownloadRuntime> map) {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry
				.getExtensionPoint(DOWNLOAD_RUNTIMES_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement configurationElement = configurationElements[j];
				String name = configurationElement.getAttribute(NAME);
				String id = configurationElement.getAttribute(ID);
				String version = configurationElement.getAttribute(VERSION);
				String url = configurationElement.getAttribute(URL);
				String disclaimer = configurationElement.getAttribute(DISCLAIMER);
				String licenseURL = configurationElement.getAttribute(LICENSE_URL);
				String size = configurationElement.getAttribute(SIZE);
				DownloadRuntime downloadRuntime = new DownloadRuntime(id, name, version, url);
				if( licenseURL != null ) {
					downloadRuntime.setLicenseURL(licenseURL);
				}
				if (Boolean.FALSE.toString().equals(disclaimer)) {
					downloadRuntime.setDisclaimer(false);
				}
				if (size != null) {
					downloadRuntime.setSize(size);
				}
				map.put(id, downloadRuntime);
			}
		}
	}
	private void loadExternalDownloadableRuntimes(HashMap<String, DownloadRuntime> map) {
		try {
			String urlString = ExternalRuntimeDownload.getURL();
			if (getCacheModified() == 0 || getRemoteModified(urlString) != getCacheModified()) {
				downloadRemoteRuntimeFile(urlString);
			}
		} catch (Exception e) {
			RuntimeCoreActivator.getDefault().logError(e);
		}
		File cacheFile = getCacheFile();
		if (cacheFile != null && cacheFile.isFile()) {
			try {
				DocumentBuilderFactory dbf = DocumentBuilderFactory
						.newInstance();
				DocumentBuilder db = dbf.newDocumentBuilder();
				Document doc = db.parse(cacheFile);
				parseRuntime(map, doc, "runtime");
			} catch (Exception e) {
				RuntimeCoreActivator.getDefault().logError(e);
			} 
		}
	}

	private void parseRuntime(HashMap<String, DownloadRuntime> map, Document doc, String tag) {
		NodeList runtimes = doc.getElementsByTagName(tag); //$NON-NLS-1$
		int len = runtimes.getLength();
		for (int i = 0; i < len; i++) {
			Node node = runtimes.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				Element element = (Element) node;
				DownloadRuntime runtime = getRuntime(element);
				if (runtime != null) {
					map.put(runtime.getId(), runtime);
				}
			}
		}
	}
	
	private DownloadRuntime getRuntime(Element element) {
		String id = element.getAttribute(ID);
		String name = element.getAttribute(NAME); 
		String version = element.getAttribute(VERSION); 
		String url = element.getAttribute(URL);
		String disclaimer = element.getAttribute(DISCLAIMER);
		String licenseUrl = element.getAttribute(LICENSE_URL);
		String size = element.getAttribute(SIZE);
		DownloadRuntime runtime = null;
		if (id == null || name == null || version == null || url == null) {
			IStatus status = new Status(IStatus.WARNING, 
					RuntimeCoreActivator.PLUGIN_ID,
					"Invalid runtime: id=" + id + ",name=" + 
							name + ",version=" + version + ",url=" + url);
			RuntimeCoreActivator.getDefault().getLog().log(status);
		} else {
			runtime = new DownloadRuntime(id, name, version, url);
			runtime.setLicenseURL(licenseUrl);
			runtime.setDisclaimer(Boolean.parseBoolean(disclaimer));
			if (size != null) {
				runtime.setSize(size);
			}
		}
		return runtime;
	}

}
