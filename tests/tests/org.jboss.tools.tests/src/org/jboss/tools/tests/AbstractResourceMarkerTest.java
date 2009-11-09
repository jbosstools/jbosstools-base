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

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.test.util.JUnitUtils;

/**
 * @author eskimo
 *
 */
public class AbstractResourceMarkerTest extends TestCase {

	/**
	 * 
	 */
	public AbstractResourceMarkerTest() {
	}

	/**
	 * 
	 */
	public AbstractResourceMarkerTest(String name) {
		super(name);
	}

	protected int findMarkerLine(IResource resource, String type, String pattern)
			throws CoreException {
		int number = -1;
		IMarker[] markers = findMarkers(resource, type, pattern);
		for (int i = 0; i < markers.length; i++) {
			number = markers[i].getAttribute(IMarker.LINE_NUMBER, -1);
		}

		return number;
	}

	protected IMarker[] findMarkers(IResource resource, String type, String pattern) throws CoreException {
		List<IMarker> result = new ArrayList<IMarker>();
		IMarker[] markers = resource.findMarkers(type, true, IResource.DEPTH_INFINITE);
		for (int i = 0; i < markers.length; i++) {
			String message = markers[i].getAttribute(IMarker.MESSAGE, ""); //$NON-NLS-1$
			if (message.matches(pattern) && markers[i].exists()) {
				result.add(markers[i]);
			}
		}
		return result.toArray(new IMarker[0]);
	}

	protected void assertMarkerIsCreated(IResource resource, MarkerData markerData) throws CoreException {
		assertMarkerIsCreated(resource, markerData.type, markerData.pattern, markerData.line);

	}

	protected void assertMarkerIsCreated(IResource resource, String type, String pattern, int expectedLine) 
		throws CoreException {

		int line = findMarkerLine(
				resource, type, pattern);

		assertTrue("Marker  matches the '" + pattern + "' pattern wasn't found",  //$NON-NLS-1$ //$NON-NLS-2$
				line != -1);

		assertEquals("Marker matches the '" + pattern + "' pattern was found at wrong line",  //$NON-NLS-1$//$NON-NLS-2$
				expectedLine,line);
	}

	protected void assertMarkerIsNotCreated(IResource resource, String type, String pattern) throws CoreException {
		IMarker[] markers = findMarkers(resource, type, pattern);

		assertFalse("Marker  matches the '" + pattern + "' pattern was found", markers.length>0); //$NON-NLS-1$ //$NON-NLS-2$
	}

	protected void assertMarkerIsNotCreated(IResource resource, String type, String pattern, int expectedLine) throws CoreException {
		int line = findMarkerLine(resource, type, pattern);

		assertFalse("Marker  matches the '" + pattern + "' pattern was found", line != -1); //$NON-NLS-1$ //$NON-NLS-2$
	}

	protected void assertMarkerIsCreated(IResource resource, String type, String pattern) throws CoreException {
		IMarker[] markers = findMarkers(resource, type, pattern);

		assertTrue("Marker  matches the '" + pattern + "' pattern wasn't found",  //$NON-NLS-1$ //$NON-NLS-2$
			markers.length>0);
	}

	protected void assertMarkersIsCreated(IResource resource, MarkerData[] markersData) throws CoreException {
		for (MarkerData markerData : markersData) {
			assertMarkerIsCreated(resource, markerData);
		}
	}

	public static int getMarkersNumber(IResource resource) {
		return getMarkersNumber(resource, null);
	}

	public static int getMarkersNumber(IResource resource, IMarkerFilter filter) {
		try{
			IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);
			int length = markers.length;
			for(int i=0;i<markers.length;i++){
//				System.out.println("Marker - "+markers[i].getAttribute(IMarker.MESSAGE, ""));  //$NON-NLS-1$//$NON-NLS-2$
				if(markers[i].exists() && (filter==null || !filter.accept(markers[i]))) {
					length--;
				}
			}
			return length;
		}catch(CoreException ex){
			JUnitUtils.fail("Can't get problem markers", ex); //$NON-NLS-1$
		}
		return -1;
	}

	public static String[] getMarkersMessage(IResource resource) {
		return getMarkersMessage(resource, null);
	}

	public static String[] getMarkersMessage(IResource resource, IMarkerFilter filter) {
		List<String> messages = new ArrayList<String>();
		try{
			IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);

//			System.out.println("Marker - "+markers[i].getAttribute(IMarker.MESSAGE, ""));  //$NON-NLS-1$//$NON-NLS-2$
			for(int i=0;i<markers.length;i++){
				if(markers[i].exists() && (filter==null || filter.accept(markers[i]))) {
					messages.add(markers[i].getAttribute(IMarker.MESSAGE, "")); //$NON-NLS-1$
				}
			}
		}catch(CoreException ex){
			JUnitUtils.fail("Can't get problem markers", ex); //$NON-NLS-1$
		}
		return messages.toArray(new String[0]);
	}

	public static Integer[] getMarkersNumbersOfLine(IResource resource) {
		return getMarkersNumbersOfLine(resource, null);
	}

	public static Integer[] getMarkersNumbersOfLine(IResource resource, IMarkerFilter filter) {
		List<Integer> numbers = new ArrayList<Integer>();
		try{
			IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);

			for(int i=0;i<markers.length;i++){
//				System.out.println("Marker line number - "+markers[i].getAttribute(IMarker.LINE_NUMBER, 0)); //$NON-NLS-1$
				if(markers[i].exists() && (filter==null || filter.accept(markers[i]))) {
					numbers.add(markers[i].getAttribute(IMarker.LINE_NUMBER, 0));
				}
			}
		}catch(CoreException ex){
			JUnitUtils.fail("Can't get problem markers.", ex); //$NON-NLS-1$
		}
		return numbers.toArray(new Integer[0]);
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