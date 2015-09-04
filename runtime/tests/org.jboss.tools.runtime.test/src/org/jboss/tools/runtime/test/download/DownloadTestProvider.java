/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.runtime.test.download;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimesProvider;

public class DownloadTestProvider implements IDownloadRuntimesProvider {

	public static final int MODE_NULL = 0;
	public static final int MODE_EMPTY = 1;
	public static final int MODE_TWO_ELEMENTS = 2;
	
	private static int MODE = MODE_NULL;
	
	public static void setMode(int m) {
		MODE = m;
	}
	
	public DownloadTestProvider() {
	}

	@Override
	public DownloadRuntime[] getDownloadableRuntimes(String requestType, IProgressMonitor monitor) {
		switch(MODE) {
			case MODE_NULL:
				return null;
			case MODE_EMPTY:
				return new DownloadRuntime[0];
			case MODE_TWO_ELEMENTS: 
				return generateTwoRuntimes();
		}
		return null;
	}

	private DownloadRuntime[] generateTwoRuntimes() {
		DownloadRuntime d1 = new DownloadRuntime("id1", "name1", "1.0.0", "http://www.example.com");
		DownloadRuntime d2 = new DownloadRuntime("id2", "name2", "2.0.0", "http://www.example.com/2/");
		return new DownloadRuntime[] { d1, d2 };
	}
}
