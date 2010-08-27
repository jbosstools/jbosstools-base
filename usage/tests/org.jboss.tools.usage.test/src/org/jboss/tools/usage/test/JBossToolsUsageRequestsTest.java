/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.HttpURLConnection;

import org.jboss.tools.usage.HttpGetRequest;
import org.jboss.tools.usage.ILoggingAdapter;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageRequestsTest {

	private ILoggingAdapter loggingAdapter;

	@Before
	public void setUp() {
		this.loggingAdapter = new SystemOutLogger();
	}

//	@Ignore
//	@Test
//	public void testUrl0() throws IOException {
//		String userAgent = "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) Firefox/10.0.0";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=338321265"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_0() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=338321288"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_0"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156032507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_1() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) v201006010437R-H98-GA";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=3383212651"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_1"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030508.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_2() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) Eclipse/3.5.0";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=3383212652"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_2"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030509.1285760712.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_3() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US;) Eclipse/3.5.0";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=3383212651"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_3"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030501.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_4() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=3383212651"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_4"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030502.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_5() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Linux x86_64)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=33832126513"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_5"
//				+ "&utmac=UA-17645367-1"
//				+ "&__utma%3D156030503.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_6() throws IOException {
//		String userAgent = "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) com.jboss.jbds.product/3.0.1";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=338321265"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_6"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=338321268"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_1() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-us)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=338321068"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmje=1"
//				+ "&utmfl=10.1%20r53"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_1"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_2() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=338333268"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=-%20JBoss%20Community"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_2"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D156620507.1285760111.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=311333268"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D112660507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_mac() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Macintosh; U; Intel Mac OS X 10.5; fr)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=351333268"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_mac"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D133660507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=351333254"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win"
//				+ "&utmhid=1087431432"
//				+ "&utmr=0"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D133660522.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_1() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=358333254"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_1"
//				+ "&utmhid=1087431432"
//				+ "&utmr=smooks|seam|drools|esb"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_1"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D133860522.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl8() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=453325272"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-US"
//				+ "&utmdt=jboss.org-tools-usage-instance"
//				+ "&utmhid=1722580305"
//				+ "&utmr=org.jboss.tools.usage.tests"
//				+ "&utmp=%2Fjboss.org%2Ftools%2Fusage%2FtestUrl8"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D999.69517276658961975851281943564260.1281943564259.1281943564259.1281943564259.-1%3B%2B__utmz%3D999.1281943564259.1.1.utmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win_referral() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=351334444"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win_referral"
//				+ "&utmhid=1087431432"
//				+ "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
//				+ "&utm_content=test1%7Ctest2%7Ctest3"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_referral"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D133663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win_adcontent() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=378334444"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win_adcontent"
//				+ "&utmhid=1087431432"
//				+ "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
//				+ "&utm_content=test1%7Ctest2%7Ctest3%7test4"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_adcontent"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D455663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win_keyword() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=378334354"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win_keyword"
//				+ "&utmhid=1087431432"
//				+ "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
//				+ "&term=test1%7Ctest2%7Ctest3%7test4"
//				+ "&utm_term=test1a%7Ctest2a%7Ctest3a%7test4a"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_keyword"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc=__utma%3D887463892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win_utmz() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=351334444"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win__utmz"
//				+ "&utmhid=1087431432"
//				+ "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
//				+ "&utm_content=test1%7Ctest2%7Ctest3"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win__utmz"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmz=test1%7Ctest2%7Ctest3"
//				+ "&utmcc=__utma%3D133663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win_utmctr() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=351334444"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win__utmctr"
//				+ "&utmhid=1087431432"
//				+ "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
//				+ "&utm_content=test1%7Ctest2%7Ctest3"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_utctr"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmz=test1%7Ctest2%7Ctest3"
//				+ "&utmcc=__utma%3D133663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3D%7Cutmctr%3Dtest1%7Ctest2%7Ctest3%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}
//
//	@Ignore
//	@Test
//	public void testUrl0_7_3_win_utmctr_lengthtest() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=351334794"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-test_0_7_3_win_lengthtest"
//				+ "&utmhid=1087431432"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_lengthtest"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc="
//				+ "__utma%3D133697892.1285760711.1281430767.1281430767.1281430767.1%3B%2B"
//				+ "__utmz%3D156030500.1281430767.1.1."
//					+ "utmcsr%3D(direct)%7C"
//					+ "utmccn%3D(direct)%7C"
//					+ "utmcmd%3D(none)%7C"
//					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}

	@Test
	public void testUrl_utmaCookies_0() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=351358794"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_0"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_0"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D133697892.111.1281430767.1281430767.1281430767.1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	/**
	 * this test should create a request to the same url by a different eclipse
	 * instance
	 * 
	 * @throws IOException
	 */
	@Test
	public void testUrl_utmaCookies_0_otherEclipse() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=351357694"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_0"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_0"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D133697892.2222.1281430767.1281430767.1281430767.1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testUrl_utmaCookies_1() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=261334794"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_1"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_1"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D133697892.1285760711.1281430767.1281430767.1281430867.2%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

//	@Test
//	public void testUrl_utmaCookies_1B() throws IOException {
//		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
//		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
//		String url = "http://www.google-analytics.com/__utm.gif?"
//				+ "utmwv=4.7.2"
//				+ "&utmn=261390794"
//				+ "&utmhn=jboss.org"
//				+ "&utmcs=UTF-8"
//				+ "&utmsr=1920x1080"
//				+ "&utmsc=24-bit"
//				+ "&utmul=en-us"
//				+ "&utmdt=tools-usage-testUrl_utmaCookies_1B"
//				+ "&utmhid=1087431432"
//				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_1B"
//				+ "&utmac=UA-17645367-1"
//				+ "&utmcc="
//				+ "__utma%3D133697892.1285760711.1281430767.1281430767.1281430867.2%3B%2B"
//				+ "__utmz%3D156030500.1281430767.1.1."
//					+ "utmcsr%3D(direct)%7C"
//					+ "utmccn%3D(direct)%7C"
//					+ "utmcmd%3D(none)%7C"
//					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
//				+ "&gaq=1";
//		method.request(url);
//		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
//	}

	@Test
	public void testUrl_utmaCookies_2() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=351784794"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_2"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_2"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D133697892.1285760711.1281430767.1281430867.1281430967.2%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testUrl_utmaCookies_utmb_utmc_1() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=351789994"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D131297892.1285760711.1281430767.1281430867.1281430967.2%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	/**
	 * this test should create a request from the same eclipse instance later in
	 * time (visit count increased, visit timestamps updated, userId identical)
	 */
	@Test
	public void testUrl_utmaCookies_utmb_utmc_1B() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=35176694"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D131297892.1285760711.1281430767.1281430967.1281430988.3%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	/**
	 * this test should create a request from the same eclipse instance later in
	 * time (visit count increased, visit timestamps updated, userId identical)
	 */
	@Test
	public void testUrl_debug_utma() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=15176694"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D999.5737734690471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testUrl_utma_utmz() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; en-US)";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent, loggingAdapter);
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=15176694"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
				+ "&utmhid=1087431432"
				+ "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+ "__utma%3D999.5737734690471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D999.1282924103925.1.1."
					+ "utmcsr%3D(direct)%7C"
					+ "utmccn%3D(direct)%7C"
					+ "utmcmd%3D(none)%7C"
					+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}
	
	protected class TestHttpGetMethod extends HttpGetRequest {

		private HttpURLConnection urlConnection;

		public TestHttpGetMethod(String userAgent, ILoggingAdapter loggingAdapter) {
			super(userAgent, loggingAdapter);
		}

		@Override
		protected HttpURLConnection createURLConnection(String urlString,
				String userAgent) throws IOException {
			return this.urlConnection = super.createURLConnection(urlString, userAgent);
		}

		public int getResponseCode() throws IOException {
			return urlConnection.getResponseCode();
		}
	}
}
