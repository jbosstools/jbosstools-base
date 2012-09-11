/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.model;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;

/**
 * 
 * @author snjeza
 *
 */
public class DownloadRuntime {
	private byte[] BUFFER = null;
	private String name;
	private String id;
	private String version;
	private String url;
	private String licenseURL;
	private boolean disclaimer = true;
	
	public DownloadRuntime(String id, String name, String version, String url) {
		super();
		this.id = id;
		this.name = name;
		this.version = version;
		this.url = url;
	}
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getVersion() {
		return version;
	}
	public void setVersion(String version) {
		this.version = version;
	}
	public String getUrl() {
		return url;
	}
	public void setUrl(String url) {
		this.url = url;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		DownloadRuntime other = (DownloadRuntime) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "DownloadRuntime [name=" + name + ", id=" + id + ", version="
				+ version + ", url=" + url + "]";
	}

	public boolean isDisclaimer() {
		return disclaimer;
	}

	public void setDisclaimer(boolean disclaimer) {
		this.disclaimer = disclaimer;
	}
	
	public void setLicenseURL(String url) {
		this.licenseURL = url;
	}

	/*
	 * @see IInstallableRuntime#getLicense(IProgressMonitor)
	 */
	public String getLicense(IProgressMonitor monitor) throws CoreException {
		URL url = null;
		ByteArrayOutputStream out = null;
		try {
			if (licenseURL == null)
				return null;
			
			url = new URL(licenseURL);
			InputStream in = url.openStream();
			out = new ByteArrayOutputStream();
			copyWithSize(in, out, null, 0);
			return new String(out.toByteArray());
		} catch (Exception e) {
			throw new CoreException(new Status(IStatus.ERROR, 
					RuntimeCoreActivator.PLUGIN_ID, 0,
					NLS.bind("Unable to fetch license for {0}", e.getLocalizedMessage()), e));
		} finally {
			try {
				if (out != null)
					out.close();
			} catch (IOException e) {
				// ignore
			}
		}
	}
	
	private void copyWithSize(InputStream in, OutputStream out, IProgressMonitor monitor, int size) throws IOException {
		if (BUFFER == null)
			BUFFER = new byte[8192];
		SubMonitor progress = SubMonitor.convert(monitor, size);
		int r = in.read(BUFFER);
		while (r >= 0) {
			out.write(BUFFER, 0, r);
			progress.worked(r);
			r = in.read(BUFFER);
		}
	}
}
