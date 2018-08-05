/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.core.model;

import static org.eclipse.core.runtime.IStatus.ERROR;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_CATALOG_SUFFIX;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_ARTIFACT_ID_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_GROUP_ID_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_MISSION_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_PREFIX;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_PROJECT_NAME_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_RUNTIME_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_RUNTIME_VERSION_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ZIP_VERSION_PARAMETER_NAME;
import static org.jboss.tools.common.launcher.core.LauncherCorePlugin.PLUGIN_ID;
import static org.jboss.tools.foundation.core.ecf.URLTransportUtility.CACHE_FOREVER;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.launcher.core.LauncherCoreConstants;
import org.jboss.tools.common.launcher.core.LauncherCorePlugin;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;

import com.fasterxml.jackson.databind.ObjectMapper;

public class CatalogManager {

	private static final CatalogManager INSTANCE = new CatalogManager();
	
	private static final ObjectMapper mapper = new ObjectMapper();
	
	private static final URLTransportUtility TRANSPORT_UTILITY = new URLTransportUtility();
	
	public static CatalogManager getDefault() {
		return INSTANCE;
	}
	
	private final Map<String, Catalog> catalogs = new HashMap<>();
	
	private CatalogManager() {}
	
	public Catalog getCatalog(IProgressMonitor monitor) throws CoreException {
		return getCatalog(LauncherCorePlugin.getDefault().getDefaultEndpointURL(), monitor);
	}
	
	public Catalog getCatalog(String endpointURL, IProgressMonitor monitor) throws CoreException {
		Catalog catalog = catalogs.get(endpointURL);
		if (catalog == null) {
			catalog = loadCatalog(endpointURL, monitor);
			catalogs.put(endpointURL, catalog);
		}
		return catalog;
	}

	private Catalog loadCatalog(String endpointURL, IProgressMonitor monitor) throws CoreException {
		if (!endpointURL.endsWith("/")) {
			endpointURL += "/";
		}
		endpointURL += LAUNCHER_CATALOG_SUFFIX;
		try {
			File file = TRANSPORT_UTILITY.getCachedFileForURL(endpointURL, endpointURL, CACHE_FOREVER, monitor);
			return mapper.readValue(file, Catalog.class);
		}
		catch (IOException ioe) {
			throw new CoreException(new Status(ERROR, PLUGIN_ID, ioe.getLocalizedMessage(), ioe));
		}
	}
	
	public IStatus zip(String endpointURL, String mission, String runtime, String runtimeVersion, String projectName,
			String groupId, String artifactId, String version, OutputStream output, IProgressMonitor monitor) {
		if (!endpointURL.endsWith("/")) {
			endpointURL += "/";
		}
		endpointURL += LAUNCHER_ZIP_PREFIX;
		endpointURL += '?';
		endpointURL += LAUNCHER_ZIP_MISSION_PARAMETER_NAME + '=' + mission + '&';
		endpointURL += LAUNCHER_ZIP_RUNTIME_PARAMETER_NAME + '=' + runtime + '&';
		endpointURL += LAUNCHER_ZIP_RUNTIME_VERSION_PARAMETER_NAME + '=' + runtimeVersion + '&';
		endpointURL += LAUNCHER_ZIP_PROJECT_NAME_PARAMETER_NAME + '=' + projectName + '&';
		endpointURL += LAUNCHER_ZIP_GROUP_ID_PARAMETER_NAME + '=' + groupId + '&';
		endpointURL += LAUNCHER_ZIP_ARTIFACT_ID_PARAMETER_NAME + '=' + artifactId + '&';
		endpointURL += LAUNCHER_ZIP_VERSION_PARAMETER_NAME + '=' + version;
		try {
			return TRANSPORT_UTILITY.download(endpointURL, endpointURL, output, monitor);
		} catch (RuntimeException e) {
			return new Status(IStatus.ERROR, LauncherCorePlugin.PLUGIN_ID, e.getLocalizedMessage(), e);
		}
	}
}