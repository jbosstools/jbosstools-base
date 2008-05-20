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
    public String GENERAL = "general";
    public String PROJECT = "project";
    public String[] LIST = {GENERAL, PROJECT};
    public String SCOPE = "SCOPE";
    public String XSTUDIO = "XStudio";
    public String GENERAL_FILE = "meta/ProgramSettingsDefault.xml";
//	public String PROJECT_PALETTE_FILE = "../../../config/Palette.xml";
//	public String PROJECT_PREFERENCES_FILE = "../../../config/Preferences.xml";
	public String PROJECT_PALETTE_FILE = "Palette.xml";
	public String PROJECT_PREFERENCES_FILE = "Preferences.xml";
    public String[][] FILE_LIST = {{GENERAL_FILE}, {PROJECT_PALETTE_FILE, PROJECT_PREFERENCES_FILE}}; 
}
