/******************************************************************************* 
 * Copyright (c) 2016 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.checkup.internal.log;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.internal.views.log.LogEntry;
import org.eclipse.ui.internal.views.log.LogSession;
import org.eclipse.ui.internal.views.log.TailInputStream;
import org.jboss.tools.foundation.checkup.FoundationCheckupPlugin;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;

public class JVMProblemLogUtil {
	private static final DateFormat DATE_FORMATTER = new SimpleDateFormat(LogEntry.F_DATE_FORMAT);
	private static final String LEGACY_DATE_STRING = "EEE MMM dd HH:mm:ss z yyyy";
	private static final DateFormat LEGACY_DATE_FORMATTER = new SimpleDateFormat(LEGACY_DATE_STRING);
	
	// in order to collect more data to show we do not show the warning dialog immediately 
	// time to wait before show the warning dialog in milliseconds
	private static final long WAIT_TIME_AFTER_READING_LOG = 7000;
	private static final long MAX_LENGTH = 1024 * 1024;
	
	
	public static Date getEntryDate(String line){
		//!ENTRY <pluginID> <severity> <code> <date>
		//!ENTRY <pluginID> <date> if logged by the framework!!!
		StringTokenizer stok = new StringTokenizer(line, LogEntry.SPACE);
		StringBuffer dateBuffer = new StringBuffer();
		int tokens = stok.countTokens();
		String token = null;
		for (int i = 0; i < tokens; i++) {
			token = stok.nextToken();
			switch (i) {
				case 0 :
					break;
				
				case 1 :
					break;
				
				case 2 :
					try {
						Integer.parseInt(token);
					} catch (NumberFormatException nfe) {
						appendToken(dateBuffer, token);
					}
					break;
				
				case 3 : 
					try {
						Integer.parseInt(token);
					} catch (NumberFormatException nfe) {
						appendToken(dateBuffer, token);
					}
					break;
				
				default : 
					appendToken(dateBuffer, token);
			}
		}
		
		return getDate(dateBuffer.toString());
	}
	
	public static void appendToken(StringBuffer buffer, String token) {
		if (buffer.length() > 0) {
			buffer.append(LogEntry.SPACE);
		}
		buffer.append(token);
	}
	
	public static Date getSessionDate(String line) {
		line = line.substring(LogSession.SESSION.length()).trim(); // strip "!SESSION "
		int delim = line.indexOf("----"); //$NON-NLS-1$
		if (delim == -1) {
			return null;
		}
		String dateBuffer = line.substring(0, delim).trim();
		return getDate(dateBuffer);
	}
	
	public static Date getDate(String dateBuffer) {
		try {
			return DATE_FORMATTER.parse(dateBuffer);
		} catch (ParseException e) {
			// current date format didn't work, try legacy
			try {
				return LEGACY_DATE_FORMATTER.parse(dateBuffer);
			} catch(ParseException e2) {
				FoundationCheckupPlugin.logError(e);
			}
		}
		return null;
	}
	
	

	/**
	 * changed to public for test purpose
	 * @param InputStream
	 */
	public static void readLogFile(JVMProblemModel model, InputStream stream) {
		LineScanner scanner = new LineScanner(model);
		BufferedReader reader = null;
		try{
			reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));  //$NON-NLS-1$
			while (true) {
				String line0 = reader.readLine();
				if (line0 == null)
					break;
				String line = line0.trim();
				scanner.scanLine(line);
			}
		} catch (FileNotFoundException e) {
			FoundationCheckupPlugin.logError(e);
		} catch (IOException e) {
			FoundationCheckupPlugin.logError(e);
		}finally{
			try {
				if(reader != null){
					reader.close();
				}
			} catch (IOException e) {
				// do nothing
			}
		}
		
		model.report(WAIT_TIME_AFTER_READING_LOG);
	}
	
	public static void reaLogModel(JVMProblemModel model) {
		// read the existing model before events start coming in:
		// ex:  read the log or read the already-collected osgi framework events
		// read error log
		File logFile = Platform.getLogFileLocation().toFile();
		if (logFile != null && logFile.exists()){
			try {
				readLogFile(model, new TailInputStream(logFile, MAX_LENGTH));
			} catch (IOException e) {
				FoundationCheckupPlugin.logError(e);
			}
		}
	}
}
