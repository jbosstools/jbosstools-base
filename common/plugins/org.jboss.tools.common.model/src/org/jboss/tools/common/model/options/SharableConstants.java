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
package org.jboss.tools.common.model.options;

public interface SharableConstants {
    public String GENERAL = "general"; //$NON-NLS-1$
    public String PROJECT = "project"; //$NON-NLS-1$
    public String[] LIST = {GENERAL, PROJECT};
    public String SCOPE = "SCOPE"; //$NON-NLS-1$
    public String XSTUDIO = "XStudio"; //$NON-NLS-1$
    public String GENERAL_FILE = "meta/ProgramSettingsDefault.xml"; //$NON-NLS-1$
//	public String PROJECT_PALETTE_FILE = "../../../config/Palette.xml";
//	public String PROJECT_PREFERENCES_FILE = "../../../config/Preferences.xml";
	public String PROJECT_PALETTE_FILE = "Palette.xml"; //$NON-NLS-1$
	public String PROJECT_PREFERENCES_FILE = "Preferences.xml"; //$NON-NLS-1$
    public String[][] FILE_LIST = {{GENERAL_FILE}, {PROJECT_PALETTE_FILE, PROJECT_PREFERENCES_FILE}};
    
    public String OPTIONS = "Options"; //$NON-NLS-1$
}
