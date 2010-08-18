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

import org.jboss.tools.usage.internal.EclipseEnvironment;

/**
 * @author Andre Dietisheim
 */
public class EclipseEnvironmentFake extends EclipseEnvironment {

	private String locale;
	private String os;

	public EclipseEnvironmentFake(String accountName, String hostName, String referral, String os, String locale) {
		super(accountName, hostName, referral);
		this.os = os;
		this.locale = locale;
	}

	@Override
	protected void initScreenSettings() {
		// do not access swt/display
	}

	@Override
	public String getScreenResolution() {
		return 1920 + SCREERESOLUTION_DELIMITER + 1080;
	}

	@Override
	public String getScreenColorDepth() {
		return 24 + SCREENCOLORDEPTH_POSTFIX;
	}

	@Override
	protected String getApplicationName() {
		return "com.jboss.jbds.product";
	}

	@Override
	protected String getNL() {
		return locale;
	}

	@Override
	protected String getOS() {
		return os;
	}
	
	@Override
	protected String getApplicationVersion() {
		return "3.0.1";
	}

}
