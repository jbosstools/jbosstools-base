/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.tests;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

/**
 * @author eskimo
 *
 */
public class AbstractResourceMarkerTest extends TestCase {

	/**
	 * 
	 */
	public AbstractResourceMarkerTest() {
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * 
	 */
	public AbstractResourceMarkerTest(String name) {
		super(name);
	}

	protected int findMarkerfLine(IFile file, String type, String pattern)
			throws CoreException {
		int number = -1;
		IMarker[] markers = new IMarker[0];

		markers = file.findMarkers(type, false, IResource.DEPTH_INFINITE);

		for (int i = 0; i < markers.length; i++) {
			String message = markers[i].getAttribute(IMarker.MESSAGE, "");
			if (message.matches(pattern)) {
				number = markers[i].getAttribute(IMarker.LINE_NUMBER, -1);
			}
		}

		return number;
	}
	
	protected void assertMarkerIsCreated(IFile file, MarkerData markerData) throws CoreException {
		assertMarkerIsCreated(file, markerData.type, markerData.pattern, markerData.line);
	}
	
	protected void assertMarkerIsCreated(IFile file, String type, String pattern, int expectedLine) 
		throws CoreException {
		
		int line = findMarkerfLine(
				file, type, pattern);
		
		assertTrue("Marker  matches the '" + pattern + "' pattern wasn't found", 
				line != -1);
		
		assertEquals("Marker matches the '" + pattern + "' pattern was found at wrong line",
				expectedLine,line);
	}
	
	protected void assertMarkersIsCreated(IFile file, MarkerData[] markersData) throws CoreException {
		for (MarkerData markerData : markersData) {
			assertMarkerIsCreated(file, markerData);
		}
	}
	
	/**
	 * 
	 * @author eskimo
	 *
	 */
	public static class MarkerData {
		
		private String type;
		private String pattern;
		private int line = -1;

		public MarkerData(String type, String pattern, int line) {
			this.type = type;
			this.pattern = pattern;
			this.line = line;
		}
		
		public int getLine() {
			return line;
		}

		public void setLine(int line) {
			this.line = line;
		}

		public String getType() {
			return type;
		}

		public void setType(String type) {
			this.type = type;
		}

		public String getPattern() {
			return pattern;
		}

		public void setPattern(String pattern) {
			this.pattern = pattern;
		}
		
	}

}
